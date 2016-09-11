(ns fhir-schema.self-test
  (:require [clojure.test :refer :all]
            [fhir-schema.core :as subj]
            [fhir-schema.utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn mk-message [x]
  (str (:id x) " " (:resourceType x) " " (subs (pr-str x) 0 100)))

(defn load-sample [nm]
  (let [rs  (map :resource (:entry (u/read-json (str "fhir-1.6/" nm ".json"))))]
    (doseq [r rs]
      (let [res (subj/validate r)]
        (when-not (empty? (:errors res))
          (spit (str "/tmp/problems/" (:id r) ".yml") (u/to-yaml r)))
        (is (empty? (:errors res)) (mk-message r))))))


(deftest self-test
  (load-sample "search-parameters")
  (load-sample "conceptmaps")
  (load-sample "v2-tables")
  (load-sample "v3-codesystems")
  (load-sample "profiles-types")
  (load-sample "profiles-resources")
  (load-sample "valuesets"))
