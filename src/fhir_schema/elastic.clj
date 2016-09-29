(ns fhir-schema.elastic
  (:require [clojure.string :as str]
            [fhir-schema.core :as fhir]
            [fhir-schema.utils :as u]
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
  (str "#/definitions/" tp))


(def transform-types
  {"BackboneElement" {:type  "object"}
   "Element" {:type  "object"}

   "Reference" {:type  "object"
                :properties {:reference {:type "string" :analyzer "keyword"}
                             :display   {:type "string"}}}

   "object"   {:type  "object"}})

(defn- calculate-types [x]
  (if-let [tp (and (map? x) (:$$type x))]
    (cond
      (:$$contentReference x) (select-keys x [:$$path :$$contentReference])

      (primitive-type? tp) (assoc x :type (:$$type x))

      :else  (if-let [tt (get transform-types tp)]
               (merge-with merge x tt)
               (assoc x :$ref (pointer-to-type tp))))
    x))


(defn- fix-elements [x]
  (cond
    ;; resource schema
    (= 1 (count (:$$path x)))
    (merge-with merge x {:properties {:resourceType {:type "string" :analyzer "keyword"}}})

    ;; references to resource
    (= (:$ref x) (pointer-to-type "Resource"))
    {:type "object"}


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

(defn path-to-pointer [x]
  (->> x (map name)
       (str/join "/")
       (str "#/definitions/")))


(defn- special? [x]
  (str/starts-with? (name x) "$$"))

(defn- clear [x]
  (if (map? x)
    (into {} (filter (fn [[k v]] (not (special? k))) x))
    x))


(def primitives {:date {:type "date"}
                 :decimal {:type "long"},
                 :id {:type "string" :analyzer "keyword"}
                 :uri {:type "string"},
                 :dateTime {:type "date"},
                 :instant {:type "date"},
                 :time {:type "date"},
                 :code {:type "string" :analyzer "keyword"},
                 :markdown {:type "string"},
                 :fhir_id {:type "string" :analyzer "keyword"},
                 :oid {:type "string", :analyzer "keyword"},
                 :xhtml {:type "string"}
                 :unsignedInt {:type "integer"},
                 :positiveInt {:type "integer"},
                 :uuid {:type "string"  :analyzer "keyword"},
                 :base64Binary {:type "binary"}})

(def additional-defs {})

(defn- element-to-schema [e]
  (let [m {:$$path (:$$path e)
           :$$type (or (get-in e [:type 0 :code]) "object")
           :$$max (if (> (count (:$$path e)) 1) (:max e) 1)}]
    (if-let [cref (:contentReference e)]
      (assoc m :$$contentReference cref)
      m)))

(defn- build-definitions [ems]
  (reduce (fn [acc e]
            (assoc-in acc (interpose :properties (:$$path e)) e))
          {} ems))


(defn resolve-ref [ref sch]
  (let [path (-> (str/replace ref #"^#/" "")
                  (str/split #"/")
                  (->> (map keyword))
                  (rest))
        resolved (get-in sch path)]
    (or resolved
        {:$unresolved path})))

(defn- es-postwalk [sch]
  (postwalk
   (fn [x]
     (cond
       (and (map? x) (:$ref x)) (resolve-ref (:$ref x) sch)
       (and (map? x) (empty? x)) {:type "object"}
       :else x)) sch))

(defn only-resources [sch]
  (reduce  (fn [acc [k v]]
             (if (re-matches #"^[A-Z].*" (name k))
               (assoc acc k (dissoc v :type))
               acc))
           {} sch))

(defn include? [{path :$$path :as x}]
  (let [l (last path)
        f (first path)]
    (not (or  (contains? #{:extension :modifierExtension} l)
              (and (= l :id) (< 1 (count path)))
              (contains? #{:Resource :Bundle :DomainResource} f)
              (str/starts-with? (name l) "_")))))

(defn generate [elems]
  (-> (->> elems
           (filter include?)
           (map element-to-schema)
           (map calculate-types)
           (build-definitions)
           (postwalk fix-elements)
           (postwalk clear))
      (-> (merge primitives additional-defs)
          (es-postwalk)
          (es-postwalk)
          (es-postwalk)
          (only-resources))))

(def es-mappings (atom nil))

(defn mappings []
  (if @es-mappings
    @es-mappings
    (reset! es-mappings (generate fhir/elements))))

(comment
  (u/save-yalm (mappings) "/tmp/schema.yml" )
  )
