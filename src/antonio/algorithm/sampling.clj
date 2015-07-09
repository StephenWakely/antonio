(ns antonio.algorithm.sampling
  (:require [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala]
            [antonio.defaults :refer [*name* *universe*]]
            [antonio.macros :refer [defprobability defalgorithm defapply]])
  (:import (com.cra.figaro.algorithm.sampling Importance$ MetropolisHastings$ ProposalScheme$)))


(defn ProposalScheme
  "Creates a proposal scheme with the element parameters.
  Pass no parameters to use the default scheme."
  ([]
   (.default ProposalScheme$/MODULE$ (*universe*)))
  ([& elements]
   (.apply ProposalScheme$/MODULE$ (apply scala/immutable-list elements))))

(defprobability Importance % ($/fn [x] (% x)))

(defprobability MetropolisHastings % ($/fn [x] (% x)))
(defalgorithm MetropolisHastings
  [% (apply scala/immutable-list %)]
  [% % (apply scala/immutable-list %)]
  [% % % (apply scala/immutable-list %)]
  [% % % % (apply scala/immutable-list %)])
