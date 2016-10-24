(defproject fhir-schema.clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths  ["src"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha12"]
                 [json-schema "0.1.0-RC2"]
                 [clj-yaml "0.4.0"]
                 [clj-antlr "0.2.3"]
                 [cheshire "5.6.3"]]
  :profiles
  {:dev {:source-paths ["src"]
         :resource-paths ["resources" "test"]
         :dependencies [[me.raynes/fs "1.4.6"]]}})
