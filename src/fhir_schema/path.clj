(ns fhir-schema.path
  (:require [clj-antlr.core :as antlr]
            [fhir-schema.utils :as u]
            [clojure.java.io :as io]
            [clojure.walk :as walk]))

;; (def parse (antlr/parser
;;             (.getPath (io/resource "grammars/fpath.g4"))))

;; (defn clear [pt]
;;   (walk/postwalk (fn [xs]
;;                    (if (seq? xs)
;;                      (remove (fn [x] (= "." x)) xs)
;;                      xs)
;;                    ) pt))

;; (u/save-edn (clear (parse "contact.where(system = z).name.empty()"))
;;              "/tmp/parse.yml")

;; (def samples (u/read-yaml "constraints.yml"))

;; (for [id (path :contained :id)]
;;   (and (not (nil? id))
;;        (contains? (path '* :reference) (str "#" id))))


;; (walk/prewalk
;;  (fn [x]
;;    (if (seq? x)
;;      (cond
;;        (= :expression (first x)) (into () (rest x))
;;        (= :invocation (first x)) (into () (rest x))
;;        (= :function (first x)) (into () (rest x))
;;        (= :identifier (first x)) (into () (rest x))
;;        (= ")" x) ""
;;        (= "(" x) ""
;;        (= "." x) ""
;;        :else x)
;;      x))

;;  (parse "contained.where(('#'+id in %resource.descendants().reference).not()).empty()")
;;  )



;; (comment 
;;   (not
;;    (empty
;;     (path 'contained
;;           (where (in (path 'id)
;;                      (path 'resource (descendants) 'reference)))))))



