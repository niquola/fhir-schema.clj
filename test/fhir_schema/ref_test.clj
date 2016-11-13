(ns fhir-schema.ref-test
  (:require [clojure.test :refer :all]
            [fhir-schema.core :as subj]
            [fhir-schema.utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def test-set
  [{:resource {:resourceType "Patient"
               :gender "male"}
    :deferreds #{{:value "male",
                  :strength "required",
                  :valueSetReference {:reference "http://hl7.org/fhir/ValueSet/administrative-gender"}
                  :type "ValueSet"
                  :path [:gender]}}}
   {:resource {:resourceType "Encounter"
               :episodeOfCare [{:reference "EpisodeOfCare/2"}
                               {:reference "EpisodeOfCare/3"}]
               :patient {:reference "Patient/1"}}
    :deferreds #{{:value {:reference "Patient/1"}
                  :type  "Reference"
                  :path  [:patient]}
                 {:value {:reference "EpisodeOfCare/2"}
                  :type  "Reference"
                  :path  [:episodeOfCare 0]}
                 {:value {:reference "EpisodeOfCare/3"}
                  :type  "Reference"
                  :path  [:episodeOfCare 1]}}}])

(deftest ref-test
  (doseq [t test-set]
    (let [res (subj/validate (:resource t))]
      (is (empty? (:errors res)))
      (is (= (:deferreds t) (:deferreds res))))))
