(ns bdrml2uzi.codegen
  (:require [clojure.string :as str]
            [middleware.parser.ast-nodes :as ast]))

(defn- as-identifier [name]
  (-> name
      str/lower-case
      (str/replace #"[^a-zA-Z0-9_]" "_")))

(defn- as-script [behavior-name]
  (ast/task-node :name (as-identifier behavior-name)
                 :body (ast/block-node [])
                 :state "stopped"))

(defn generate-code [{:keys [behaviors]}]
  (ast/program-node
   :scripts (mapv as-script behaviors)))
