(ns fhir-schema.postgres-test
  (:require [fhir-schema.postgres :as sut]
            [clojure.java.jdbc :as jdbc]
            [clj-pg.errors :refer [pr-error]]
            [clj-pg.coerce]
            [clojure.test :refer [deftest is]]))

(def conn {:connection-uri (System/getenv "DATABASE_URL")})

(defn exec [& args]
  (pr-error (apply jdbc/execute! conn args)))

(defn q [& args]
  (pr-error (apply jdbc/query conn args)))

(defn q-first [& args]
  (first (apply q args)))

(defn q-value [& args]
  (first (vals (apply q-first args))))

(deftest schema-test
  (exec [(sut/deinit-sql)])
  (exec [(sut/init-sql)])
  (exec [(sut/drop-storage-sql "Patient")])
  (exec [(sut/storage-sql "Patient")])

  (is (not (empty? (q ["SELECT * from information_schema.tables where table_name = 'patient'"]))))


  (let [res {:resourceType "Patient" :name [{:text "Ivan Ivanov"}]}
        create-sql (sut/create-resource-sql {:resource res})
        created (q-first create-sql)]
    (is (not (nil? created))))

  (is (= 0 (q-value "SELECT count(*) FROM patient_history")))

  (let [created (q-first "SELECT * FROM patient")
        updated (q-first (sut/update-resource-sql {:resource (merge {:id (:id created)
                                                                     :resourceType (:resource_type created)}
                                                                    (assoc-in (:resource created) [:name 0 :text] "changed"))}))]
    (is (not (nil? (:id created))))
    (is (= (:id created) (:id updated)))
    (is (= 1 (q-value "SELECT count(*) FROM patient_history")))

    (is (= (:id created) (:id (q-first (sut/delete-resource-sql created)))))

    (is (= 2 (q-value "SELECT count(*) FROM patient_history"))))

  (exec [(sut/drop-storage-sql "Patient")])

  (is (empty? (q ["SELECT * from information_schema.tables where table_name = 'patient'"]))))

