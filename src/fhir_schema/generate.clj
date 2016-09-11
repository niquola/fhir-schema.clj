(ns fhir-schema.generate
  (:require [clojure.string :as str]
            [clojure.walk :refer :all]))

(defn- array? [x] (= "*" (:$$max x)))

(defn- primitive-type? [x]
  (contains? #{"string" "integer" "number" "boolean"} x))

(defn- calculate-requireds [x]
  (if (map? x)
    (let [required (->> (filter (fn [[k v]] (= 1 (:$$min v))) x)
                        (map first))]
      (if (empty? required) x (assoc x :required required)))
    x))

(defn pointer-to-type [tp]
  (str "#/definitions/" (if (= "id" tp) "fhir_id" tp)))


(def transform-types
  {"BackboneElement" {:type  "object"
                      :minProperties 1
                      :additionalProperties false
                      :properties {:extension {:$ref (pointer-to-type "ArrayOfExtensions")}
                                   :modifierExtension {:$ref (pointer-to-type "ArrayOfExtensions")}}}
   "Element" {:type  "object"
              :minProperties 1
              :additionalProperties false
              :properties {:extension {:$ref (pointer-to-type "ArrayOfExtensions")}}}

   "Reference" {:type  "object"
                :minProperties 1
                :additionalProperties false
                :$deffered {:type "Reference"}
                :required [:reference]
                :properties {:reference {:type "string"}
                             :display {:type "string"}}}

   "object"   {:type  "object"
               :minProperties 1
               :additionalProperties false}})

(defn- calculate-types [x]
  (if-let [tp (and (map? x) (:$$type x))]
    (cond
      (:$$contentReference x) (select-keys x [:$$path :$$contentReference])

      (primitive-type? tp) (assoc x :type (:$$type x))

      :else  (if-let [tt (get transform-types tp)]
               (merge-with merge x tt)
               (assoc x :$ref (pointer-to-type (if (= "id" tp) "fhir_id" tp)))))
    x))


(defn- calculate-arrays [x]
  (if (array? x)
    (-> {:type "array" :items x}
        (merge (if (= 0 (:$$min x)) {} {:minItems (:$$min x)})))
    x))

(defn- fix-elements [x]
  (cond
    ;; resource schema
    (= 1 (count (:$$path x)))
    (merge-with merge x {:properties {:resourceType {:type "string" :constant (name (get-in x [:$$path 0]))}}})

    ;; references to resource
    (= (:$ref x) (pointer-to-type "Resource"))
    {:type "object" :typeProperty "resourceType"}

    ;; valueset binding
    (:$$binding x)
    (assoc x :$deffered (-> (select-keys (:$$binding x) [:strength :valueSetReference])
                            (assoc  :type "ValueSet")))

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

(defn path-to-pointer [x]
  (->> x
       (map name)
       (str/join "/")
       (str "#/definitions/")))

(defn- global-postwalk [sch]
  (postwalk
   (fn [x]
     (if-let [cr (:$$contentReference x)]
       {:$ref (-> cr
                  (resolve-content-ref sch)
                  (path-to-pointer))}
       x))
   sch))

(defn- special? [x]
  (str/starts-with? (name x) "$$"))

(defn- clear [x]
  (if (map? x)
    (into {} (filter (fn [[k v]] (not (special? k))) x))
    x))


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
                        :properties {:extension {:$ref (pointer-to-type "ArrayOfExtensions")}}}
   :ArrayOfExtensions {:type "array"
                       :items {:$ref (pointer-to-type "Extension")}}})

(defn- element-to-schema [e]
  {:$$description (:short e)
   :$$binding (:binding e)
   :$$contentReference (:contentReference e)
   :$$path (:$$path e)
   :$$type (or (get-in e [:type 0 :code]) "object")
   :$$min (:min e)
   :$$max (if (> (count (:$$path e)) 1) (:max e) 1)})

(defn- build-definitions [ems]
  (reduce (fn [acc e]
            (assoc-in acc (interpose :properties (:$$path e)) e))
          {} ems))

(defn generate
  "elems - collection of FHIR element definitions"
  [elems]
  {:type "object"
   :typeProperty :resourceType 
   :definitions
   (-> (->> elems
            (map element-to-schema)
            (map calculate-types)
            (map calculate-requireds)
            (build-definitions)
            (postwalk calculate-arrays)
            (postwalk fix-elements)
            (global-postwalk)
            (postwalk clear))
       (-> (dissoc :required)
           (merge primitives additional-defs)))})
