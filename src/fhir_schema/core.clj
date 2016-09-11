(ns fhir-schema.core
  (:require
   [fhir-schema.generate :as gen]
   [fhir-schema.utils :as u]
   [clojure.string :as str]
   [clojure.walk :refer :all]
   [json-schema.core :as schema]))


(defn- poly-type? [x] (re-seq #"\[x]" x))

(defn update-last [f xs]
  (conj (into [] (butlast xs))
        (f (last xs))))

(defn substitute-x [nm tp]
  (str/replace (name nm) #"\[x\]" (u/camelize (name tp))))


(defn- expand [{pth :$$path :as e}]
  (let [x (last pth)]
    (if (poly-type? (name x))
      (map (fn [tp]
             (merge e {:$$path (update-last #(keyword (substitute-x (name %) (:code tp)))
                                            pth)
                       :type [tp]}))
           (:type e))
      [e])))

(defn primitive-extensions [e]
  (let [tp (get-in e [:type 0 :code])]
    (if (and tp (re-matches #"^[a-z].*$" tp))
      [e (merge e {:$$path (update-last #(keyword (str "_" (name %))) (:$$path e)) 
                   :type [{:code "PrimitiveExtension"}]
                   :min 0})]
      [e])))

(def elements (->>
               (concat (map :resource (:entry (u/read-json "fhir-1.6/profiles-resources.json")))
                       (map :resource (:entry (u/read-json "fhir-1.6/profiles-types.json"))))
               (filter #(= "StructureDefinition" (:resourceType %)))
               (mapcat #(get-in % [:snapshot :element]))
               (map (fn [e] (assoc e :$$path (mapv keyword (str/split (:path e) #"\.")))))
               (mapcat expand)
               (mapcat primitive-extensions)))

(def schema (gen/generate elements))

(defn validate [res]
  (schema/validate schema res))

(comment
  (schema/validate schema {:id "x" :resourceType "Patient" :name [{}]})
  (u/save-yalm schema "/tmp/schema.yml" )
  )

