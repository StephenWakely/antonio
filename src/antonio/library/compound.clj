(ns antonio.library.compound
  (:require [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala]
            [antonio.macros :refer [defapply]]
            [antonio.defaults :refer [*name* *universe*]])
  (:import (com.cra.figaro.library.compound CPD$)))


(defapply CPD
  [% (apply scala/immutable-list (map (fn [cond->element] (apply $/tuple cond->element)) %))]
  [% % (apply scala/immutable-list (map (fn [cond->element] (apply $/tuple cond->element)) %))]
  [% % % (apply scala/immutable-list (map (fn [cond->element] (apply $/tuple cond->element)) %))]
  [% % % % (apply scala/immutable-list (map (fn [cond->element] (apply $/tuple cond->element)) %))]
  [% % % % % (apply scala/immutable-list (map (fn [cond->element] (apply $/tuple cond->element)) %))])
