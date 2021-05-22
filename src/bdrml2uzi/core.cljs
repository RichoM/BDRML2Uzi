(ns bdrml2uzi.core
  (:require [bdrml2uzi.parser :as p]
            [bdrml2uzi.codegen :as cg]))

(defn init []
  (println "Hello World"))

(defn ^:export parse [src]
  (clj->js (p/parse src)))

(defn ^:export codegen [src]
  (-> src
      p/parse
      cg/generate-code))
