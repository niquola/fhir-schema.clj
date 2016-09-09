(defproject fhir-schema.clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths  ["src"
                  "json-schema.clj/src"
                  "json-schema.clj/test"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha12"]
                 [clj-yaml "0.4.0"]
                 [cheshire "5.6.3"]]
  :profiles
  {:dev {:source-paths ["src" "json-schema.clj/src" "json-schema.clj/test"] 
         :resource-paths ["resources" "test"]}})
