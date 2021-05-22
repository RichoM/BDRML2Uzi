(ns bdrml2uzi.codegen
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [middleware.parser.ast-nodes :as ast]
            [middleware.code-generator.code-generator :as uzi]))

(def ^:const empty-block (ast/block-node []))

(defn- index-of [s v]
  (loop [idx 0 items s]
    (cond
      (empty? items) nil
      (= v (first items)) idx
      :else (recur (inc idx) (rest items)))))

(defn- as-identifier [name]
  (-> name
      str/lower-case
      (str/replace #"[^a-zA-Z0-9_]" "_")))

(defn- enforce-unique-identifiers [bdrml]
  (let [names (volatile! {})
        temp (w/postwalk (fn [o]
                           (if (string? o)
                             (if-let [uuid (@names o)]
                               uuid
                               (let [uuid (random-uuid)]
                                 (vswap! names assoc o uuid)
                                 uuid))
                             o))
                         bdrml)
        used-names (volatile! #{})
        unique-name (fn [name]
                      (let [id (as-identifier name)]
                        (if (contains? @used-names id)
                          (recur (str name "_"))
                          (do
                            (vswap! used-names conj id)
                            id))))
        identifiers (into {}
                          (map (fn [[name uuid]]
                                 [uuid (unique-name name)])
                               @names))]
    (w/postwalk (fn [o]
                  (if-let [id (identifiers o)]
                    id
                    o))
                temp)))

(defn- behavior-as-script [name]
  (ast/task-node :name name
                 :body empty-block
                 :state "stopped"))

(defn- data-as-script [name]
  (ast/function-node :name name
                     :body empty-block))

(defmulti condition-data :type)
(defmethod condition-data :existence [{:keys [data]}] data)
(defmethod condition-data :non-existence [{:keys [data]}] data)
(defmethod condition-data :boolean [{:keys [predicate]}] predicate)
(defmethod condition-data :textual [{:keys [text]}] text)
(defmethod condition-data :always [_] nil)

(defn generate-condition-call [condition]
 (case (:type condition)
   :always (ast/literal-number-node 1)
   :non-existence (ast/call-node "!"
                                 [(ast/arg-node (ast/call-node (condition-data condition) []))])
   (ast/call-node (condition-data condition) [])))

(defn generate-composed-condition-call [[head & tail]]
  (if head
    (reduce (fn [subtotal next-item]
            (ast/logical-or-node
             subtotal
             (generate-condition-call next-item)))
          (generate-condition-call head)
          tail)
    (ast/literal-number-node 1)))

(defn- generate-state-block [index behavior bdrml]
  (ast/conditional-node
   (ast/call-node "==" [(ast/arg-node (ast/variable-node "state"))
                        (ast/arg-node (ast/literal-number-node index))])
   (ast/block-node
    (vec (concat
          [(ast/resume-node [behavior])
           (ast/yield-node)]

          (map (fn [{:keys [conditions to]}]
                 (ast/conditional-node
                  (generate-composed-condition-call conditions)
                  (ast/block-node
                   [(ast/call-node "transitions.push"
                                   [(ast/arg-node
                                     (ast/literal-number-node
                                      (index-of (:behaviors bdrml)
                                                to)))])])))
               (filter (fn [{:keys [type from]}]
                         (and (= :transition type)
                              (= behavior from)))
                       (:relations bdrml)))

          [(ast/conditional-node
            (ast/call-node ">" [(ast/arg-node (ast/call-node "transitions.count" []))
                                (ast/arg-node (ast/literal-number-node 0))])
            (ast/block-node
             [(ast/assignment-node
               (ast/variable-node "state")
               (ast/call-node "transitions.get_random" []))
              (ast/stop-node [behavior])]))
           (ast/return-node)])))))

(defn generate-loop-task [{:keys [behaviors external-data relations] :as bdrml}]
  (ast/task-node
   :name "loop"
   :state "running"
   :body (ast/block-node
          (vec (concat [(ast/call-node "transitions.clear" [])]
                       (map-indexed (fn [index behavior]
                                      (generate-state-block index behavior bdrml))
                                    behaviors))))))

(defn generate-ast [bdrml]
  (let [{:keys [behaviors external-data relations] :as bdrml} (enforce-unique-identifiers bdrml)]
    (ast/program-node
     :globals [(ast/variable-declaration-node "state")]
     :imports [(ast/import-node "transitions" "List.uzi"
                                (ast/block-node
                                 [(ast/assignment-node
                                   (ast/variable-node "size")
                                   (ast/literal-number-node (dec (count behaviors))))]))]
     :scripts (vec (concat
                    [(generate-loop-task bdrml)]
                    (map behavior-as-script behaviors)
                    (map data-as-script
                         (set (keep condition-data
                                    (mapcat :conditions relations)))))))))

(defn generate-code [bdrml]
  (-> bdrml
      generate-ast
      uzi/print))


(comment

(enforce-unique-identifiers bdrml)
(def bdrml (bdrml2uzi.parser/parse
            "B = {Buscar, buscar, Ataque frontal, Retroceder}
             De = {Oponente  adelante: bool, Linea blanca detectada: bool}
             trans(Buscar, Retroceder) : {∃ Linea blanca detectada}
             trans(Retroceder, buscar) : {Tr elapsed}
             trans(Buscar, Ataque frontal) : {∃ Oponente  adelante}
             trans(Ataque frontal, buscar) : {∄ Oponente  adelante}
             trans(Ataque frontal, Retroceder) : {∃ Linea blanca detectada}"))

(conj #{} [1 2 3 4])
(apply set [1 2 3] [2 3 4])
(into #{}
      [1 2 3])

 ,,)
