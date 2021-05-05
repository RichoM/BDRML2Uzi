(ns bdrml2uzi.parser
  (:require [petitparser.core :as pp]))

(def grammar
  {:start (pp/end :model)
   :model (pp/plus (pp/or :behaviors :data-structures))
   :behaviors [(pp/trim "B") (pp/trim "=") (pp/trim "{")
               (pp/separated-by :behavior-name (pp/trim ","))
               (pp/trim "}")]
   :behavior-name :identifier
   :data-structures [(pp/trim (pp/or "De" "Di")) (pp/trim "=") (pp/trim "{")
                    (pp/separated-by :data-def (pp/trim ","))
                    (pp/trim "}")]
   :data-def [:identifier (pp/trim ":") (pp/plus pp/letter)]
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
                       (vec (take-nth 2 data))})})

(def parser (pp/compose grammar transformations))

(defn parse [src] (pp/parse parser src))

(comment
  (set! *print-length* 100)
  (take-nth 2 (range 0 10))
  (pp/parse parser "")
  (pp/parse parser
            "B={Avanzar, Buscar, Retroceder}
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
