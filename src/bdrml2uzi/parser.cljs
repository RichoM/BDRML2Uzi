(ns bdrml2uzi.parser
  (:require [petitparser.core :as pp]
            [clojure.string :as str]))

(defn trim-seq [& parsers]
  (mapv pp/trim parsers))

(def grammar
  {:start (pp/end :model)
   :model (pp/plus (pp/or :behaviors :data-structures :relations))
   :behaviors (trim-seq "B" "=" "{" (pp/separated-by :identifier (pp/trim ",")) "}")
   :data-structures (trim-seq (pp/or "De" "Di") "=" "{"
                              (pp/separated-by :data-def (pp/trim ","))
                              "}")
   :data-def [:identifier (pp/trim ":") (pp/plus pp/letter)]
   :relations (pp/or :transition :read :write :receive :send :copy :update)
   :transition (trim-seq "trans" "(" :identifier "," :identifier ")" ":"
                         "{" :conditions "}")
   :read (trim-seq "read" "(" :identifier "," :identifier ")" ":"
                         "{" :conditions "}")
   :write (trim-seq "write" "(" (pp/optional (trim-seq :value ":")) :identifier "," :identifier ")" ":"
                    "{" :conditions "}")
   :receive (trim-seq "receive" "(" :identifier "," :identifier ")" ":"
                   "{" :conditions "}")
   :send (trim-seq "send" "(" (pp/optional (trim-seq :value ":")) :identifier "," :identifier ")" ":"
                    "{" :conditions "}")
   :copy (trim-seq "copy" "(" :identifier "," :identifier ")" ":"
                   "{" :conditions "}")
   :update "TODO"
   :conditions (pp/separated-by (pp/or :always-cond :boolean-cond :textual-cond
                                       :existence-cond :non-existence-cond)
                                (pp/trim ","))
   :always-cond "*"
   :boolean-cond :identifier
   :textual-cond (trim-seq "\"" :identifier "\"")
   :existence-cond (trim-seq "∃" :identifier)
   :non-existence-cond (trim-seq "∄" :identifier)
   :value (pp/flatten (pp/plus-greedy pp/any
                                      (trim-seq ":" :identifier)))
   :identifier (pp/flatten (pp/plus (pp/or pp/word pp/space)))})

(def transformations
  {:model (fn [prims]
            (reduce (partial merge-with into) prims))
   :behaviors (fn [[_ _ _ behaviors _]]
                {:behaviors (vec (take-nth 2 behaviors))})
   :data-def (fn [[name _ type]] name)
   :data-structures (fn [[scope _ _ data _]]
                      {(case scope
                         "De" :external-data
                         "Di" :internal-data)
                       (vec (take-nth 2 data))})
   :relations (fn [rel] {:relations [rel]})
   :transition (fn [[_ _ from _ to _ _ _ conditions _]]
                 {:type :transition, :from from, :to to,
                  :conditions conditions})
   :read (fn [[_ _ data _ behavior _ _ _ conditions _]]
           {:type :read, :data data, :behavior behavior,
            :conditions conditions})
   :write (fn [[_ _ [value _] data _ behavior _ _ _ conditions _]]
           {:type :write, :data data, :behavior behavior,
            :value value, :conditions conditions})
   :receive (fn [[_ _ data _ behavior _ _ _ conditions _]]
           {:type :receive, :data data, :behavior behavior,
            :conditions conditions})
   :send (fn [[_ _ [value _] data _ behavior _ _ _ conditions _]]
           {:type :send, :data data, :behavior behavior,
            :value value, :conditions conditions})
   :copy (fn [[_ _ from _ to _ _ _ conditions _]]
           {:type :copy, :from from, :to to,
            :conditions conditions})
   :conditions (fn [conditions] (vec (take-nth 2 conditions)))
   :always-cond (constantly {:type :always})
   :boolean-cond (fn [p] {:type :boolean, :predicate p})
   :textual-cond (fn [[_ text _]] {:type :textual, :text text})
   :existence-cond (fn [[_ data]] {:type :existence, :data data})
   :non-existence-cond (fn [[_ data]] {:type :non-existence, :data data})
   :value str/trim
   :identifier str/trim})

(def parser (pp/compose grammar transformations))

(defn parse [src] (pp/parse parser src))

(comment

  (pp/parse (pp/flatten (pp/plus (pp/negate ",")))
            "ac,bd,")

 (pp/parse (pp/negate ",")
           ",a")

 *e

  (set! *print-length* 100)
  (take-nth 2 (range 0 10))
  (pp/parse parser "")
  (pp/parse parser
            "trans(Retroceder, Buscar) : {Tr elapsed}
B={Avanzar, Buscar, Retroceder}
             Di={Oponente adelante: bool , Linea detectada : bool}
             De= {a :int, b: long}
             De= {c :int, d: long}")
*e
  (reduce (partial merge-with into)
          [{:a [1 2 3]}
           {:b [4 5 6]}
           {:a [7 8 9]}])
  (merge-with into
              (merge-with into {:a [1]}
                               {:b 2})
              {:a 3})
 *e

 (pp/parse (-> parser :parsers :value)
           ":asdf")

  (pp/parse (-> parser :parsers :write)
            "write (+1 : a , b ) : {*} ")

 (merge-with into
             {:a 1 :b 2}
             )

  (merge-with into
           	  {"Lisp" ["Common Lisp" "Clojure"]
           	   "ML" ["Caml" "Objective Caml"]}
           	  {"Lisp" ["Scheme"]
           	   "ML" ["Standard ML"]})

 "
 B = {Buscar, Retroceder, Ataque frontal}
 De = {Oponente  adelante: bool, Linea blanca detectada: bool}
 trans(Buscar, Retroceder) : {∃ Linea blanca detectada}
 trans(Retroceder, Buscar) : {Tr elapsed}
 trans(Buscar, Ataque frontal) : {∃ Oponente  adelante}
 trans(Ataque frontal, Buscar) : {∄ Oponente  adelante}
 trans(Ataque frontal, Retroceder) : {∃ Linea blanca detectada}"
 ,)
