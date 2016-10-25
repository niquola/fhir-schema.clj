(ns fhir-schema.postgres
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [honeysql.core :as hsql]))

(defn to-json [x]
  (json/generate-string x))

(defn init-sql []
"
CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS resource (
  id text,
  version_id integer,
  resource_type text,
  resource jsonb,
  created_at timestamp with time zone,
  updated_at timestamp with time zone
);

CREATE TABLE IF NOT EXISTS resource_history (
  id text,
  version_id integer,
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
(defn to-hx-table-name [x] (str (str/lower-case x) "_history"))

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
ADD PRIMARY KEY (id, version_id),
ALTER COLUMN valid_from SET NOT NULL,
ALTER COLUMN valid_to SET NOT NULL,
ALTER COLUMN resource SET NOT NULL,
ALTER COLUMN resource_type SET DEFAULT '%s';
   ", table-name, table-name, resource-type, table-name, table-name, resource-type)))


(defn drop-storage-sql [resource-type]
  (let [table-name (to-table-name resource-type)]
    (format "DROP TABLE IF EXISTS %s ; DROP TABLE IF EXISTS %s_history" table-name table-name)))

(defn create-resource-sql [{{rt :resourceType :as resource} :resource}]
  [(format  "
    WITH ids as ( SELECT gen_random_uuid() as uuid )
    INSERT INTO %s (id, version_id, resource, created_at, updated_at)
    VALUES ((SELECT uuid FROM ids LIMIT 1), 1, ?, current_timestamp, current_timestamp)
    RETURNING *"
    (to-table-name rt))
   (dissoc resource :id :resourceType)])

(defn update-resource-sql [{{id :id rt :resourceType :as resource} :resource}]
  [(format  "
      WITH history AS (
        INSERT INTO %s_history (id, version_id, resource, valid_from, valid_to)
        SELECT id, version_id, resource, updated_at, current_timestamp
        FROM %s
        WHERE id = ?
        RETURNING *
      )
      UPDATE %s SET
        resource = ?,
        version_id = version_id + 1,
        updated_at = current_timestamp
      WHERE id = ?
      RETURNING *
    " (to-table-name rt) (to-table-name rt) (to-table-name rt))
   id (to-json (dissoc resource :id :resourceType)) id])

(defn delete-resource-sql [{id :id rt :resource_type}]
  [(format  "
      WITH history AS (
        INSERT INTO %s_history (id, version_id, resource, valid_from, valid_to)
        SELECT id, version_id, resource, updated_at, current_timestamp
        FROM %s
        WHERE id = ?
        RETURNING *
      )
      DELETE FROM %s
      WHERE id = ?
      RETURNING *
    " (to-table-name rt) (to-table-name rt) (to-table-name rt))
   id  id])


(comment

  (create-resource-sql {:resource {:resourceType "Patient" :id "ups"}})
  (update-resource-sql {:resource {:resourceType "Patient" :id "ups"}})

  (spit "/tmp/sql.sql"
        (str (init-sql)
             (storage-sql "Patient"))))

