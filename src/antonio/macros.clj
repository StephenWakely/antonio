(ns antonio.macros
  (:require [clojure.walk :refer [prewalk-replace]]
            [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala]
            [antonio.defaults :refer [*name* *universe*]]))

(defn tuple
  "Make it nicer to create scala tuples"
  [& vals]
  (apply $/tuple vals))

(defn gen-params
  "Given a list of parameters in the form '(% (dosomething  %)) returns a list of vectors of generated symbols to forms with the %n replaced by the symbol"
  [params]
  (map (fn [param]
         (let [sym (gensym)]
           [sym (prewalk-replace {'% sym} param)]))
       params))

(defmacro defprobability
  [figarofn & params]
  (let [gparams (gen-params params)]
    `(defn ~(symbol (str figarofn "_prob"))
       [~@(map first gparams)]
       (.probability ~(symbol (str figarofn "$/MODULE$"))
                     ~@(map second gparams)))))

(defn overload-defn
  "Creates the overloaded component of the generated function."
  [figarofn defaults params]
  (let [gparams (gen-params params)]
    `([~@(map first gparams)]
      (.apply ~(symbol (str figarofn "$/MODULE$"))
              ~@(map second gparams)
              ~@defaults))))

(defmacro defapply
  [figarofn & params]
  `(defn ~figarofn
     ~@(map (partial overload-defn figarofn '(*name* *universe*)) params)))


(defmacro defalgorithm
  "Creates the method to call apply on Figaro algorithm functions.
  Algorithm functions only take a universe default param."
  [figarofn & params]
  `(defn ~figarofn
     ~@(map (partial overload-defn figarofn '(*universe*)) params)))

