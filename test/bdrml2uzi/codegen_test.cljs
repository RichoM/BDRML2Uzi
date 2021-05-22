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

(def empty-body (ast/block-node []))

(deftest behaviors-are-mapped-to-stopped-tasks
  (is (equivalent? (ast/program-node
                    :scripts #{(ast/task-node :name "buscar"
                                              :body empty-body
                                              :state "stopped")
                               (ast/task-node :name "retroceder"
                                              :body empty-body
                                              :state "stopped")
                               (ast/task-node :name "ataque_frontal"
                                              :body empty-body
                                              :state "stopped")})
                   (cg/generate-code BDRML))))

(deftest external-data-are-mapped-to-functions
  (is (equivalent? (ast/program-node
                    :scripts #{(ast/function-node :name "oponente__adelante"
                                                  :body empty-body)
                               (ast/function-node :name "linea_blanca_detectada"
                                                  :body empty-body)})
                   (cg/generate-code BDRML))))
