(ns fhir-schema.postgres-test
  (:require [fhir-schema.postgres :as sut]
            [clojure.java.jdbc :as jdbc]
            [clojure.test :refer [deftest is]]))

(def conn {:connection-uri (System/getenv "DATABASE_URL")})

(defn exec [& args]
  (apply jdbc/execute! conn args))

(defn q [& args]
  (apply jdbc/query conn args))

(deftest schema-test
  (exec [(sut/deinit-sql)])
  (exec [(sut/init-sql)])
  (exec [(sut/storage-sql "Patient")])

  (is (not (empty? (q ["SELECT * from information_schema.tables where table_name = 'patient'"]))))

  (exec [(sut/drop-storage-sql "Patient")])

  (is (empty? (q ["SELECT * from information_schema.tables where table_name = 'patient'"]))))


