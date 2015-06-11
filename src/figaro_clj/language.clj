(ns figaro-clj.language
  (:require [clojure.reflect :as r]
            [clojure.pprint :as pp]
            [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala]
            [figaro-clj.defaults :refer [*name* *universe*]]
            [figaro-clj.macros :refer [defapply]])
  (:import (com.cra.figaro.language Flip$ Select$ Universe$ Name$ Constant$ Chain$ Dist$ Apply$)
           (com.cra.figaro.library.compound If$)))


(defapply Flip [%])
(defapply Constant [%])
(defapply If [% ($/fn [] %) ($/fn [] %)])
(defapply Chain [% ($/fn [] %)])

(defapply Dist
  [(apply scala/immutable-list
          (map (fn [[prob outcome]] ($/tuple (double prob) outcome)) %))])

(defapply Select
  [(apply scala/immutable-list
          (map (fn [[prob outcome]] ($/tuple (Constant prob) outcome)) %))])

(defapply Apply
  [% ($/fn [a] (% a))]
  [% % ($/fn [a b] (% a b))]
  [% % % ($/fn [a b c] (% a b c))]
  [% % % % ($/fn [a b c d] (% a b c d))]
  [% % % % % ($/fn [a b c d e] (% a b c d e))])

(defapply Chain
  [% ($/fn [a] (% a))]
  [% % ($/fn [a b] (% a b))])

(defn observe
  [element value]
  (.observe element value))

(defn testme
  []
  (reduce (fn [possession idx]
            (conj possession (If (possession (dec idx))
                                  (Flip 0.6)
                                  (Flip 0.3))))
          [(Flip 0.5)]
          (range 1 90)))
