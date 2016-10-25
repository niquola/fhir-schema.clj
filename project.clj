(defproject fhir-schema.clj "0.0.1-RC1"
  :description "FHIR as a library"
  :url "https://github.com/niquola/fhir-schema"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repositories   [["clojars"  {:url "https://clojars.org/repo" :sign-releases false}]]

  :source-paths  ["src"]
  :dependencies [[org.clojure/clojure "1.9.0-alpha12"]
                 [json-schema "0.1.0-RC2"]
                 [clj-yaml "0.4.0"]
                 [clj-antlr "0.2.4"]
                 [honeysql "0.8.1"]
                 [cheshire "5.6.3"]]
  :profiles
  {:dev {:source-paths ["src"]
         :resource-paths ["resources" "test"]
         :plugins [[lein-ancient "0.6.10"]]
         :dependencies [[me.raynes/fs "1.4.6"]
                        [org.clojure/java.jdbc "0.6.1"]
                        [clj-pg "0.0.1-RC1"]
                        [org.postgresql/postgresql "9.4.1211.jre7"]]}})
