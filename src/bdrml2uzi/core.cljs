(ns bdrml2uzi.core
  (:require [bdrml2uzi.parser :as p]))

(defn init []
  (println "Hello World"))

(defn ^:export parse [src]
  (clj->js (p/parse src)))
