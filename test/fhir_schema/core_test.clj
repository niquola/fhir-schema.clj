(ns fhir-schema.core-test
  (:require [clojure.test :refer :all]
            [fhir-schema.core :as subj]
            [fhir-schema.utils :as u]
            [clojure.java.io :as io]))

(def invalid (read-string (slurp (io/resource "fhir_schema/invalid-resources.edn"))))
(def valid (read-string (slurp (io/resource "fhir_schema/valid-resources.edn"))))


(deftest valid-test
  (doseq [r valid]
    (is (empty? (:errors (subj/validate r))) (pr-str r))))

(defn load-sample [nm]
  (map :resource (:entry (u/read-json (str "fhir-1.6/" nm ".json")))))

(deftest self-test
  (doseq [r (load-sample "search-parameters")]
    (is (empty? (:errors (subj/validate r))) (pr-str r)))

  (doseq [r (load-sample "profiles-types")]
    (is (empty? (:errors (subj/validate r))) (pr-str r)))

  (doseq [r (load-sample "valuesets")]
    (is (empty? (:errors (subj/validate r))) (pr-str r)))

  )

(deftest invalid-test
  (doseq [i invalid]
    (let [res (subj/validate (:resource i))
          err-idx (reduce (fn [acc e] (assoc acc (:path e) e))
                          {} (:errors res))]
      (is (not (empty? (:errors res))) (pr-str i))
      (doseq [ep (or (:error-paths i) [])]
        (is (contains? err-idx ep) (pr-str ep))))))
