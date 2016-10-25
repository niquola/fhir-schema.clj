(ns fhir-schema.postgres
  (:require [clojure.string :as str]))

(defn init-sql []
"
CREATE TABLE resource (
  id text,
  version_id text,
  resource_type text,
  resource jsonb,
  created_at timestamp with time zone,
  updated_at timestamp with time zone
);

CREATE TABLE resource_history (
  id text,
  version_id text,
  resource_type text,
  resource jsonb,
  valid_from timestamp with time zone,
  valid_to timestamp with time zone
);
")

(defn deinit-sql []
"
DROP TABLE IF EXISTS resource CASCADE;
DROP TABLE IF EXISTS resource_history CASCADE;
")

(defn to-table-name [x] (str/lower-case x))

(defn storage-sql
  "Generate schema SQL for resource tables"
  [resource-type]
  (let [table-name (to-table-name resource-type)]
    (format
     "
CREATE TABLE %s () INHERITS (resource); 

ALTER TABLE %s
ADD PRIMARY KEY (id),
ALTER COLUMN created_at SET NOT NULL,
ALTER COLUMN created_at SET DEFAULT CURRENT_TIMESTAMP,
ALTER COLUMN updated_at SET NOT NULL,
ALTER COLUMN updated_at SET DEFAULT CURRENT_TIMESTAMP,
ALTER COLUMN resource SET NOT NULL,
ALTER COLUMN resource_type SET DEFAULT '%s';

CREATE TABLE %s_history () INHERITS (resource_history);

ALTER TABLE %s_history
ADD PRIMARY KEY (version_id),
ALTER COLUMN valid_from SET NOT NULL,
ALTER COLUMN valid_to SET NOT NULL,
ALTER COLUMN resource SET NOT NULL,
ALTER COLUMN resource_type SET DEFAULT '%s';
   ", table-name, table-name, resource-type, table-name, table-name, resource-type)))


(defn drop-storage-sql [resource-type]
  (let [table-name (to-table-name resource-type)]
    (format "DROP TABLE %s; DROP TABLE %s_history" table-name table-name)))

(comment 
  (spit "/tmp/sql.sql"
        (str (init-sql)
             (storage-sql "Patient")))
  )

