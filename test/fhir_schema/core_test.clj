(ns fhir-schema.core-test
  (:require [clojure.test :refer :all]
            [fhir-schema.core :as subj]
            [fhir-schema.utils :as u]
            [clojure.java.io :as io]
            [me.raynes.fs :as fs]
            [clojure.string :as str]))

(def invalid (read-string (slurp (io/resource "fhir_schema/invalid-resources.edn"))))

(def valid (read-string (slurp (io/resource "fhir_schema/valid-resources.edn"))))

(deftest valid-test
  (doseq [r valid]
    (is (empty? (:errors (subj/validate r))) (pr-str r))))

(defn mk-message [x]
  (str (:id x) " " (:resourceType x) " " (subs (pr-str x) 0 100)))

(defn read-sample [f]
  (let [content (slurp f)
        ex (fs/extension f)
        readers {".yaml" u/from-yaml
                 ".yml" u/from-yaml
                 ".json" u/from-json}
        reader (get readers ex)]
    (if reader
      (reader content)
      (throw (Exception. (str  "Could not read " (.getName f)))))))

(def p-files (file-seq (io/file (io/resource "fhir_schema/problematic"))))
;;flt "diagnostic-request-stage.yml"
;;files (filter (fn [x] (= (.getName x) flt)) files)

(deftest problematics-test
  (doseq [f p-files]
    (when (fs/file? f)
      (let [resource (read-sample f)]
        (println (.getName f))
        (is (empty? (:errors (subj/validate resource)))
            (.getName f))))))

(subj/validate {:resourceType "Patient" :name [{:given ["ups"]}]})

(deftest invalid-test
  (doseq [i invalid]
    (let [res (subj/validate (:resource i))
          err-idx (reduce (fn [acc e] (assoc acc (:path e) e)) {} (:errors res))]
      (is (not (empty? (:errors res))) (pr-str i))
      (doseq [ep (or (:error-paths i) [])]
        (is (contains? err-idx ep) (pr-str ep))))))
