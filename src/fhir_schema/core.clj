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

(defn- array? [x] (= "*" (second (:$$cardinality x))))

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
                   :type [{:code "PrimitiveExtension"}]
                   :binding nil
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


(comment 
  (:contentReference (first (filter (fn [x] (= (:$$path x) [:Conformance :rest :searchParam])) ems)))
  )

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
      (:$$contentReference x) {:$$contentReference (:$$contentReference x)}
      (primitive-type? tp) (assoc x :type (:$$type x))
      (= "BackboneElement" tp) (merge-with merge x {:type  "object"
                                                    :minProperties 1
                                                    :additionalProperties false
                                                    :properties {:extension {:$ref "#/definitions/ArrayOfExtensions"}
                                                                 :modifierExtension {:$ref "#/definitions/ArrayOfExtensions"}}})

      (= "Element" tp) (merge-with merge x {:type  "object"
                                            :minProperties 1
                                            :additionalProperties false
                                            :properties {:extension {:$ref "#/definitions/ArrayOfExtensions"}}})

      (= "Reference" tp) (merge-with merge x {:type  "object"
                                              :minProperties 1
                                              :additionalProperties false
                                              :$deffered {:type "Reference"}
                                              :required [:reference]
                                              :properties {:reference {:type "string"}
                                                           :display {:type "string"}}})
      (= "object" tp) (merge x {:type  "object"
                                :minProperties 1
                                :additionalProperties false})
      :else (assoc x :$ref (str "#/definitions/" (if (= "id" tp) "fhir_id" tp))))
    x))


(defn- items [x]
  (if (array? x)
    (merge {:type "array"
            :items x}
           (if (= 0 (first (:$$cardinality x)))
             {} {:minItems (first (:$$cardinality x))}))
    x))

(defn- fix-elements [x]
  (cond
    (= 1 (count (:$$path x))) (merge-with merge x {:properties {:resourceType {:type "string" :constant (name (get-in x [:$$path 0]))}}})
    (= (:$ref x) "#/definitions/Resource") {:type "object" :typeProperty "resourceType"}
    (:$$binding x) (assoc x :$deffered (assoc (select-keys (:$$binding x) [:strength :valueSetReference]) :type "ValueSet"))
    :else x))

(defn resolve-content-ref [ref sch]
  (let [path (-> (str/replace ref #"^#" "")
                 (str/split #"\.")
                 (->> (mapv keyword)))]
    (loop [node sch
           pointer []
           [k & ks] path]
      (let [nnode (get node k)
            pointer (conj pointer k)]
        (cond
          (nil? nnode) (throw (Exception. (str "Could not resolve " ref)))
          (empty? ks)  pointer
          (get-in nnode [:properties]) (recur (:properties nnode)
                                              (into [] (conj pointer :properties))
                                              ks)

          (get-in nnode [:items :properties]) (recur (get-in nnode [:items :properties])
                                                     (into [] (concat pointer [:items :properties]))
                                              ks)
          :else (throw (Exception. (str "Could not resolve " ref))))))))

(comment
  (resolve-content-ref
   "#Conformance.rest.resource.searchParam"
   (:definitions schema)))

(defn- global-postwalk [sch]
  (postwalk (fn [x]
              (cond
                (:$$contentReference x) {:$ref
                                         (->> (resolve-content-ref (:$$contentReference x) sch)
                                             (map name)
                                             (str/join "/")
                                             (str "#/definitions/"))}
                :else x))
            sch))

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
          (global-postwalk)
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

(def additional-defs
  {:PrimitiveExtension {:type "object"
                        :additionalProperties false
                        :minProperties 1
                        :properties {:extension {:$ref "#/definitions/ArrayOfExtensions"}}}
   :ArrayOfExtensions {:type "array"
                       :items {:$ref "#/definitions/Extension"}}})

(def schema {:type "object"
             :typeProperty :resourceType 
             :definitions (dissoc (merge (generate-schema) primitives additional-defs) :required)})

(spit "/tmp/schema.yml" (u/to-yaml schema))

(schema/validate schema {:id "x" :resourceType "Patient" :name [{}]})

(defn validate [res]
  (schema/validate schema res))
