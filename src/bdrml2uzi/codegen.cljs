(ns bdrml2uzi.codegen
  (:require [clojure.string :as str]
            [middleware.parser.ast-nodes :as ast]))

(def ^:const empty-block (ast/block-node []))

(defn- as-identifier [name]
  (-> name
      str/lower-case
      (str/replace #"[^a-zA-Z0-9_]" "_")))

(defn- behavior-as-script [name]
  (ast/task-node :name (as-identifier name)
                 :body empty-block
                 :state "stopped"))

(defn- data-as-script [name]
  (ast/function-node :name (as-identifier name)
                     :body empty-block))

(defn generate-code [{:keys [behaviors external-data]}]
  (ast/program-node
   :scripts (concat
             (mapv behavior-as-script behaviors)
             (mapv data-as-script external-data))))
