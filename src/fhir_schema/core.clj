(ns fhir-schema.core
  (:require
   [clojure.string :as cs]
   [clojure.set :as cset]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [fhir-schema.utils :as u]
   [clojure.string :as str]
   [clojure.walk :refer :all]
   [json-schema.core :as schema]))


(defn- poly-type? [x] (re-seq #"\[x]" x))

(defn- expand [{pth :$$path :as e}]
  (let [x (last pth)]
    (if (poly-type? (name x))
      (map (fn [tp]
             (merge e {:$$path (conj (into [] (butlast pth)) (keyword (str/replace (name x) #"\[x\]" (u/camelize (:code tp)))))
                       :type [tp]}))
           (:type e))
      [e])))

(defn primitive-extensions [e]
  (if-let [tp (get-in e [:type 0 :code])]
    (if (re-matches #"^[a-z].*$" tp)
      [e (merge e {:$$path (conj (into [] (butlast (:$$path e))) (keyword (str "_" (name (last (:$$path e))))))
                   :type [{:code "Element"}]
                   :min 0})]
      [e])
    [e]))

(def ems (->>
          (concat (map :resource (:entry (u/read-json "fhir-1.6/profiles-resources.json")))
                  (map :resource (:entry (u/read-json "fhir-1.6/profiles-types.json"))))
          (filter (fn [tp] (fn [x] (= (:resourceType "StructureDefinition") tp))))
          (mapcat (fn [x] (get-in x [:snapshot :element])))
          (map (fn [e] (assoc e :$$path (mapv keyword (str/split (:path e) #"\.")))))
          (reduce (fn [acc x] (into acc (expand x))) [])
          (reduce (fn [acc x] (into acc (primitive-extensions x))) [])))


(filter (fn [x] (= (:$$path x) [:ElementDefinition :type :code])) ems)

(defn- requireds [x]
  (if (map? x)
    (let [required (->> (filter (fn [[k v]] (= 1 (get-in v [:$$cardinality 0]))) x)
                        (map first))]
      (if (empty? required) x (assoc x :required required)))
    x))

(defn- primitive-type? [x]
  (contains? #{"string" "integer" "number" "boolean"} x))

(defn- types-to-refs [x]
  (if-let [tp (and (map? x) (:$$type x))]
    (cond
      ;; TODO do not handle items
      (:$$contentReference x) {:$ref (->
                                      (:$$contentReference x)
                                      (str/replace #"#" "")
                                      (str/split #"\.")
                                      (->> (interpose "properties")
                                           (str/join "/")
                                           (str "#/definitions/")))}
      (primitive-type? tp) (assoc x :type (:$$type x))
      (= "BackboneElement" tp) (merge-with merge x {:type  "object"
                                                    :additionalProperties false
                                                    :properties {;;:id {:$ref "#/definitions/fhir_id"}
                                                                 :extension {:type "array"
                                                                             :items {:$ref "#/definitions/Extension"}}
                                                                 :modifierExtension {:type "array"
                                                                                     :items {:$ref "#/definitions/Extension"}}}})

      (= "Element" tp) (merge-with merge x {:type  "object"
                                            :additionalProperties false
                                            :properties {;;:id {:$ref "#/definitions/fhir_id"}
                                                         :extension {:type "array"
                                                                     :items {:$ref "#/definitions/Extension"}}}})
      (= "object" tp) (merge x {:type  "object" :additionalProperties false})
      :else (assoc x :$ref (str "#/definitions/" (if (= "id" tp) "fhir_id" tp))))
    x))

(defn- array? [x] (= "*" (second (:$$cardinality x))))

(defn- items [x]
  (if (and (map? x) (array? x))
    (merge {:type "array"
            :items x}
           (if (= 0 (first (:$$cardinality x)))
             {} {:minItems (first (:$$cardinality x))}))
    x))

(defn- fix-elements [x]
  (cond
    (= 1 (count (:$$path x))) (merge-with merge x {:properties {:resourceType {:type "string" :constant (name (get-in x [:$$path 0]))}}})
    (= (:$ref x) "#/definitions/Resource") {:type "object" :typeProperty "resourceType"}
    :else x))

(defn- clear [x]
  (if (map? x)
    (into {} (filter (fn [[k v]] (not (str/starts-with? (name k) "$$"))) x))
    x))

(defn- generate-schema []
  (let [sch (reduce (fn [acc e]
                      (let [d {:$$description (:short e) 
                               :$$binding (:binding e)
                               :$$contentReference (:contentReference e)
                               :$$path (:$$path e)
                               :$$type (or (get-in e [:type 0 :code]) "object")
                               :$$cardinality [(:min e) (if (> (count (:$$path e)) 1) (:max e) 1)]}]
                        (assoc-in acc (interpose :properties (:$$path e)) d)))
                    {} ems)]

    (->>  sch
          (postwalk types-to-refs)
          (postwalk requireds)
          (postwalk items)
          (postwalk fix-elements)
          (postwalk clear))))

(def primitives {:date {:type "string" :pattern "-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1]))?)?"}
                 :decimal {:type "number"},
                 :uri {:type "string"},
                 :dateTime {:type "string",:pattern "-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?([Z+-]((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?"},
                 :instant {:type "string" :pattern "-?[0-9]{4}(-(0[1-9]|1[0-2])(-(0[0-9]|[1-2][0-9]|3[0-1])(T([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?([Z+-]((0[0-9]|1[0-3]):[0-5][0-9]|14:00))?)?)?)?"},
                 :time {:type "string" :pattern "([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9](\\.[0-9]+)?"},
                 :code {:type "string" :pattern "[^\\s]+([\\s]+[^\\s]+)*"},
                 :markdown {:type "string"},
                 :fhir_id {:type "string" :pattern "[A-Za-z0-9\\-\\.]{1,64}"},
                 :oid {:type "string", :pattern "urn:oid:[0-2](\\.[1-9]\\d*)+"},
                 :xhtml {:type "string"}
                 :unsignedInt {:type "integer", :minimum 0, :exclusiveMinimum true},
                 :positiveInt {:type "integer", :minimum 0, :exclusiveMinimum true},
                 :uuid {:type "string"},
                 :base64Binary {:type "string", :media {:binaryEncoding "base64"}}})

(def schema {:type "object"
             :typeProperty :resourceType 
             :definitions (dissoc (merge (generate-schema) primitives) :required)})

(spit "/tmp/schema.yml" (u/to-yaml schema))

(schema/validate schema {:id "x" :resourceType "Patient" :name [{}]})

(defn validate [res]
  (schema/validate schema res))
