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
                   (cg/generate-ast BDRML))))

(deftest external-data-are-mapped-to-functions
  (is (equivalent? (ast/program-node
                    :scripts #{(ast/function-node :name "oponente__adelante"
                                                  :body empty-body)
                               (ast/function-node :name "linea_blanca_detectada"
                                                  :body empty-body)})
                   (cg/generate-ast BDRML))))

(deftest all-conditions-are-mapped-to-functions
  (is (equivalent? (ast/program-node
                    :scripts #{(ast/function-node :name "tr_elapsed"
                                                  :body empty-body)
                               (ast/function-node :name "oponente__adelante"
                                                  :body empty-body)
                               (ast/function-node :name "linea_blanca_detectada"
                                                  :body empty-body)})
                   (cg/generate-ast BDRML))))

(deftest generated-program-should-import-the-list-library
  (is (equivalent? (ast/program-node
                    :imports [(ast/import-node "transitions" "List.uzi"
                                               (ast/block-node
                                                [(ast/assignment-node
                                                  (ast/variable-node "size")
                                                  (ast/literal-number-node 2))]))])
                   (cg/generate-ast BDRML))))

(deftest generated-program-should-declare-global-variable-state
  (is (equivalent? (ast/program-node
                    :globals [(ast/variable-declaration-node "state")])
                   (cg/generate-ast BDRML))))

(deftest transitions-are-mapped-to-a-loop-task-state-machine
  (is (equivalent?
       (ast/program-node
        :scripts
        #{(ast/task-node
           :name "loop"
           :body (ast/block-node
                  [(ast/call-node "transitions.clear" [])
                   (ast/conditional-node
                    (ast/call-node "==" [(ast/arg-node (ast/variable-node "state"))
                                         (ast/arg-node (ast/literal-number-node 0))])
                    (ast/block-node
                     [(ast/resume-node "buscar")
                      (ast/yield-node)
                      (ast/conditional-node
                       (ast/call-node "linea_blanca_detectada" [])
                       (ast/block-node [(ast/call-node "transitions.push"
                                                       [(ast/arg-node (ast/literal-number-node 1))])]))
                      (ast/conditional-node
                       (ast/call-node "oponente__adelante" [])
                       (ast/block-node [(ast/call-node "transitions.push"
                                                       [(ast/arg-node (ast/literal-number-node 2))])]))
                      (ast/conditional-node
                       (ast/call-node ">" [(ast/arg-node (ast/call-node "transitions.count" []))
                                           (ast/arg-node (ast/literal-number-node 0))])
                       (ast/block-node
                        [(ast/assignment-node
                          (ast/variable-node "state")
                          (ast/call-node "transitions.get_random" []))
                         (ast/stop-node "buscar")]))
                      (ast/return-node)]))]))})
       (cg/generate-ast BDRML))))
