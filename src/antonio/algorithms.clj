(ns antonio.algorithms
  (:require [t6.from-scala.core :refer [$ $$] :as $]
            [antonio.macros :refer [defprobability]])
  (:import (com.cra.figaro.algorithm.factored VariableElimination$)
           (com.cra.figaro.algorithm.sampling Importance$)))

(defprobability Importance % ($/fn [x] (% x)))
(defprobability VariableElimination % %)
