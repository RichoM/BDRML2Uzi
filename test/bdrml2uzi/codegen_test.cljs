(ns bdrml2uzi.codegen-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [clojure.data :as d]
            [bdrml2uzi.utils :refer [equivalent?]]
            [bdrml2uzi.parser :as p]
            [bdrml2uzi.codegen :as cg]
            [middleware.parser.ast-nodes :as ast]))

(def BDRML (p/parse "B = {Buscar, Retroceder, Ataque frontal}
                     De = {Oponente  adelante: bool, Linea blanca detectada: bool}
                     trans(Buscar, Retroceder) : {∃ Linea blanca detectada}
                     trans(Retroceder, Buscar) : {Tr elapsed}
                     trans(Buscar, Ataque frontal) : {∃ Oponente  adelante}
                     trans(Ataque frontal, Buscar) : {∄ Oponente  adelante}
                     trans(Ataque frontal, Retroceder) : {∃ Linea blanca detectada}"))

(deftest behaviors-are-mapped-to-stopped-tasks
  (is (equivalent? (ast/program-node
                    :scripts #{(ast/task-node :name "buscar"
                                              :body (ast/block-node [])
                                              :state "stopped")
                               (ast/task-node :name "retroceder"
                                              :body (ast/block-node [])
                                              :state "stopped")
                               (ast/task-node :name "ataque_frontal"
                                              :body (ast/block-node [])
                                              :state "stopped")})
                   (cg/generate-code BDRML))))
