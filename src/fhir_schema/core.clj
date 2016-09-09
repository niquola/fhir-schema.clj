(ns fhir-schema.core
  (:require
   [clojure.string :as cs]
   [clojure.set :as cset]
   [clojure.java.io :as io]
   [cheshire.core :as json]
   [clojure.string :as str]))

(defn read-json [pth]
  (-> (io/resource pth)
      (slurp)
      (json/parse-string  keyword)))

(defn to-json [m]
  (json/generate-string m {:pretty true}))

(defn from-json [s]
  (json/parse-string s keyword))


(defn camelize [^String s]
  (str (cs/upper-case (subs s 0 1)) (subs s 1 (.length s))))

(defn unencode-html-entities [s]
  (-> (cs/replace s #"&nbsp;" "&#160;")
      (cs/replace #"&(trade|copy|sect|reg);" "$1")))


(defn normalize-string [s]
  (cs/replace s #"\s" ""))

(def resources
  (concat
   (map :resource (:entry (read-json "fhir-1.6/profiles-resources.json")))
   (map :resource (:entry (read-json "fhir-1.6/profiles-types.json")))))

(into #{} (map :resourceType resources))


(let [by-type (fn [tp] (fn [x] (= (:resourceType x) tp)))]
  (def sd (filter (by-type "StructureDefinition") resources))
  (def od (filter (by-type "OperationDefinition") resources))
  (def conf (filter (by-type "Conformance") resources))
  (def cd (filter (by-type "CompartmentDefinition") resources)))

{:cd (count cd)
 :od (count od)
 :sd (count sd)
 :conf (count conf)}



(defn poly-type? [x] (re-seq #"\[x]" x))

(defn expand [{pth :$path :as e}]
  (let [x (last pth)]
    (if (poly-type? (name x))
      (map (fn [tp]
             (merge e {:$path (conj (into [] (butlast pth)) (str/replace x #"\[x\]" (camelize (:code tp))))
                       :type [tp]}))
           (:type e))
      [e])))

(def ems* (map (fn [e] (assoc e :$path (map keyword (str/split (:path e) #"\."))))
              (mapcat (fn [x] (get-in x [:snapshot :element])) sd)))

(def ems (reduce (fn [acc x] (into acc (expand x))) [] ems*))

(def ems-idx (reduce (fn [acc x] (assoc acc (:path x) x)) {} ems))


(map [])

(defn do-lighter [x]
  (dissoc x :comments :requirements :definition :path :mapping))

(comment
  (take 10 (keys ems-idx))
  (count ems)
  (map :path (take 10 ems))
  (dissoc (get ems-idx "Patient.identifier")
          :comments :requirements :definition)
  (def sd-meta (-> (fn [x] (= "StructureDefinition" (:type x)))
                   (filter sd)
                   first))
  (:id (second sd))
  (first sd))

(get ems-idx "HumanName.given")

(defn resource-schema [s]
  {:properties {:resourceType {:constant (:type s)}
                :required [:resourceType]}})

(do-lighter (get ems-idx "Patient.name"))

(defn generate-schema []
  (reduce (fn [acc s] (assoc acc (:type s) (resource-schema s)))
          {} sd))

(spit "/tmp/schema.json"
      (to-json {:definitions (generate-schema)}))

(def fhir-schema
  (->>
   (reduce
    (fn [acc x] (assoc-in acc (:$path x) {:$meta (do-lighter x)}))
    {} ems)))

(spit "/tmp/tempo.json"
      (to-json fhir-schema))

(first ems)

(defn validate* [errors doc vpath path obj]
  (let [sch (get-in fhir-schema path)]
       (println "VALIDATE:" vpath path sch obj)
       (cond
         (map? obj) (reduce (fn [acc [k v]]
                              (if-let [tp (get-in sch [:$meta :type 0])]
                                (validate* acc doc (conj vpath k) [(keyword (:code tp)) k] v)
                                (validate* acc doc (conj vpath k) (conj path k) v)))
                            errors
                            obj)

         (vector? obj) (reduce (fn [acc [k v]]
                              (validate* acc doc (conj vpath k) path v))
                            errors
                            (map (fn [v i] [i v]) obj (range)))
         :else errors)))

(defn validate [r]
  (validate* [] r [(keyword (:resourceType r))] [(keyword (:resourceType r))] (dissoc r :resourceType)))

(validate
  {:resourceType "Patient"
   :name [{:given ["Nicola"]}]})

