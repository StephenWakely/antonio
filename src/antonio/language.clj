(ns antonio.language
  (:require [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala]
            [antonio.defaults :refer [*name* *universe*]]
            [antonio.macros :refer [defapply]])
  (:import (com.cra.figaro.language Flip$ Select$ Universe$ Name$ Constant$
                                    NonCachingChain$ CachingChain$ Chain$ Dist$ Apply$
                                    Inject$)
           (com.cra.figaro.library.compound If$)))


(defapply Flip [(if (number? %) (double %) %)])
(defapply Constant [(if (number? %) (double %) %)])
(defapply If [% ($/fn [] %) ($/fn [] %)])

(defapply Chain [% ($/fn [] %)])
(defapply NonCachingChain
  [% ($/fn [a] (% a))]
  [% % ($/fn [a b] (% a b))] )
(defapply CachingChain
  [% ($/fn [a] (% a))]
  [% % ($/fn [a b] (% a b))] )

(defapply Dist
  [(apply scala/immutable-list
          (map (fn [[prob outcome]] ($/tuple (double prob) outcome)) %))])

(defapply Select
  [(apply scala/immutable-list
          (map (fn [[prob outcome]]
                 ($/tuple (if (number? prob) (Constant prob) prob)
                          outcome)) %))])

(defapply Apply
  [% ($/fn [a] (% a))]
  [% % ($/fn [a b] (% a b))]
  [% % % ($/fn [a b c] (% a b c))]
  [% % % % ($/fn [a b c d] (% a b c d))]
  [% % % % % ($/fn [a b c d e] (% a b c d e))])

(defapply Chain
  [% ($/fn [a] (% a))]
  [% % ($/fn [a b] (% a b))])

(defapply Inject
  [(apply scala/immutable-list %)])

(def Universe Universe$/MODULE$)
