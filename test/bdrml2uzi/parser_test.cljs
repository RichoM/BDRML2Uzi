(ns bdrml2uzi.parser-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [bdrml2uzi.parser :as p]))

(deftest behaviors
  (is (= {:behaviors ["Avanzar"]}
         (p/parse "B = {Avanzar}")))
  (is (= {:behaviors ["Avanzar" "Retroceder" "Buscar"]}
         (p/parse "B={Avanzar, Retroceder, Buscar}"))))

(deftest internal-data
  (is (= {:internal-data ["Oponente adelante"]}
         (p/parse "Di = {Oponente adelante: bool}")))
  (is (= {:internal-data ["Oponente adelante" "Linea blanca" "Color del piso"]}
         (p/parse "Di={Oponente adelante:bool, Linea blanca : bool, Color del piso:int}"))))

(deftest external-data
  (is (= {:external-data ["Oponente adelante"]}
         (p/parse "De = {Oponente adelante: bool}")))
  (is (= {:external-data ["Oponente adelante" "Linea blanca" "Color del piso"]}
         (p/parse "De ={Oponente adelante:bool, Linea blanca : bool, Color del piso:int}"))))

(deftest transitions
  (is (= {:relations [{:type :transition, :from "A", :to "B"
                       :conditions [{:type :always}]}]}
         (p/parse "trans(A, B): {*}")))
  (is (= {:relations [{:type :transition, :from "A", :to "B"
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "trans( A,B): {\"at home\"}")))
  (is (= {:relations [{:type :transition, :from "A", :to "B"
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "trans( A ,B ): {f}")))
  (is (= {:relations [{:type :transition, :from "A", :to "B"
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "trans( A ,B ): {f}")))
  (is (= {:relations [{:type :transition, :from "Buscar", :to "Retroceder"
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "trans(Buscar, Retroceder) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :transition, :from "Ataque frontal", :to "Buscar"
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "trans( Ataque frontal,Buscar ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :transition, :from "A", :to "B"
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "trans( A,B ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :transition, :from "A", :to "B"
                       :conditions nil}]}
         (p/parse "trans( A,B ) "))))

(deftest reads
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions [{:type :always}]}]}
         (p/parse "read(counter, Work): {*}")))
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "read(counter , Work): {\"at home\"}")))
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "read( counter, Work ): {f}")))
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "read( counter ,Work ): {f}")))
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "read(counter,Work) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "read( counter , Work ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "read( counter,Work ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :read, :data "counter", :behavior "Work",
                       :conditions nil}]}
         (p/parse "read( counter,Work )"))))

(deftest writes-without-data
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :always}]}]}
         (p/parse "write(counter, Work): {*}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "write(counter , Work): {\"at home\"}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "write( counter, Work ): {f}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "write( counter ,Work ): {f}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "write(counter,Work) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "write( counter , Work ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "write( counter,Work ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions nil}]}
         (p/parse "write( counter,Work )"))))

(deftest writes-with-data
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "+1",
                       :conditions [{:type :always}]}]}
         (p/parse "write(+1: counter, Work): {*}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "f",
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "write(f : counter , Work): {\"at home\"}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "f",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "write( f:counter, Work ): {f}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "+100:::",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "write( +100::::counter ,Work ): {f}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "cualquier saraza es valida",
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "write(cualquier saraza es valida:counter,Work) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "+1000",
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "write(     +1000    : counter , Work ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "-FSDFSDFSDF",
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "write(-FSDFSDFSDF: counter,Work ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :write, :data "counter", :behavior "Work",
                       :value "-FSDFSDFSDF",
                       :conditions nil}]}
         (p/parse "write(-FSDFSDFSDF: counter,Work ) "))))

(deftest receives
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions [{:type :always}]}]}
         (p/parse "receive(counter, Work): {*}")))
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "receive(counter , Work): {\"at home\"}")))
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "receive( counter, Work ): {f}")))
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "receive( counter ,Work ): {f}")))
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "receive(counter,Work) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "receive( counter , Work ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "receive( counter,Work ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :receive, :data "counter", :behavior "Work",
                       :conditions nil}]}
         (p/parse "receive( counter,Work ) "))))


(deftest sends-without-data
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :always}]}]}
         (p/parse "send(counter, Work): {*}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "send(counter , Work): {\"at home\"}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "send( counter, Work ): {f}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "send( counter ,Work ): {f}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "send(counter,Work) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "send( counter , Work ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "send( counter,Work ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value nil,
                       :conditions nil}]}
         (p/parse "send( counter,Work ) "))))

(deftest sends-with-data
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "+1",
                       :conditions [{:type :always}]}]}
         (p/parse "send(+1: counter, Work): {*}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "f",
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "send(f : counter , Work): {\"at home\"}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "f",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "send( f:counter, Work ): {f}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "+100:::",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "send( +100::::counter ,Work ): {f}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "cualquier saraza es valida",
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "send(cualquier saraza es valida:counter,Work) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "+1000",
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "send(     +1000    : counter , Work ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "-FSDFSDFSDF",
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "send(-FSDFSDFSDF: counter,Work ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :send, :data "counter", :behavior "Work",
                       :value "-FSDFSDFSDF",
                       :conditions nil}]}
         (p/parse "send(-FSDFSDFSDF: counter,Work ) "))))

(deftest copies
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions [{:type :always}]}]}
         (p/parse "copy(counter, external counter): {*}")))
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "copy(counter , external counter): {\"at home\"}")))
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "copy( counter, external counter ): {f}")))
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "copy( counter ,external counter ): {f}")))
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "copy(counter,external counter) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "copy( counter , external counter ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "copy( counter,external counter ) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :copy, :from "counter", :to "external counter",
                       :conditions nil}]}
         (p/parse "copy( counter,external counter ) "))))


(deftest updates-without-data
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions [{:type :always}]}]}
         (p/parse "update(counter): {*}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "update(counter ): {\"at home\"}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "update( counter ): {f}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "update( counter ): {f}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "update(counter) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "update( counter  ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "update( counter) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value nil,
                       :conditions nil}]}
         (p/parse "update( counter) "))))

(deftest updates-with-data
  (is (= {:relations [{:type :update, :data "counter",
                       :value "+1",
                       :conditions [{:type :always}]}]}
         (p/parse "update(+1: counter): {*}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value "f",
                       :conditions [{:type :textual, :text "at home"}]}]}
         (p/parse "update(f : counter ): {\"at home\"}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value "f",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "update( f:counter ): {f}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value "+100:::",
                       :conditions [{:type :boolean, :predicate "f"}]}]}
         (p/parse "update( +100::::counter ): {f}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value "cualquier saraza es valida",
                       :conditions [{:type :existence, :data "Linea blanca detectada"}]}]}
         (p/parse "update(cualquier saraza es valida:counter) : {∃ Linea blanca detectada}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value "+1000",
                       :conditions [{:type :non-existence, :data "Oponente adelante"}]}]}
         (p/parse "update(     +1000    : counter  ) : {∄Oponente adelante}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value "-FSDFSDFSDF",
                       :conditions [{:type :non-existence, :data "Foo"}
                                    {:type :existence, :data "Bar"}
                                    {:type :boolean, :predicate "Baz"}
                                    {:type :textual, :text "Wow"}]}]}
         (p/parse "update(-FSDFSDFSDF: counter) : {∄ Foo, ∃ Bar, Baz, \"Wow\"}")))
  (is (= {:relations [{:type :update, :data "counter",
                       :value "-FSDFSDFSDF",
                       :conditions nil}]}
         (p/parse "update(-FSDFSDFSDF: counter) "))))
