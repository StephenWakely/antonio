(ns antonio.algorithm.factored
  (:require [t6.from-scala.core :refer [$ $$] :as $]
            [antonio.macros :refer [defprobability]])
  (:import (com.cra.figaro.algorithm.factored VariableElimination$)))

(defprobability VariableElimination % %)
