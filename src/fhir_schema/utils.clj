(ns fhir-schema.utils
  (:require
   [clojure.string :as cs]
   [clojure.set :as cset]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [clojure.pprint :as pp]
   [clj-yaml.core :as yaml]
   [clojure.string :as str]))

(defn read-json [pth]
  (-> (io/resource pth)
      (slurp)
      (json/parse-string  keyword)))


(defn to-json [m]
  (json/generate-string m {:pretty true}))

(defn to-yaml [m]
  (yaml/generate-string m))

(defn save-yalm [m pth]
  (spit pth (to-yaml m)))

(defn save-edn [m pth]
  (spit pth (with-out-str (pp/pprint m))))

(defn from-yaml [s]
  (yaml/parse-string s))

(defn read-yaml [pth]
  (-> (io/resource pth)
      (slurp)
      (from-yaml)))

(defn from-json [s]
  (json/parse-string s keyword))


(defn camelize [^String s]
  (str (cs/upper-case (subs s 0 1)) (subs s 1 (.length s))))

(defn unencode-html-entities [s]
  (-> (cs/replace s #"&nbsp;" "&#160;")
      (cs/replace #"&(trade|copy|sect|reg);" "$1")))


(defn normalize-string [s]
  (cs/replace s #"\s" ""))

