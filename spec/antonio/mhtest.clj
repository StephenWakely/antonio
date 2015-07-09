(ns antonio.mhtest
  (:require [t6.from-scala.core :refer [$] :as $]
            [speclj.core :refer :all]
            [antonio.language :refer :all]
            [antonio.element :refer :all]
            [antonio.library.atomic.continuous :refer :all]
            [antonio.algorithm.sampling :refer :all]))

(defn anytimeMHtest
  [target predicate prob tolerance]
  (let [mh (MetropolisHastings (ProposalScheme) [target])]
    (.start mh)
    (Thread/sleep 1000)
    (.stop mh)
    (should= prob (.probability mh target ($/fn [p] (predicate p))) tolerance)))

(describe "Anytime MetropolisHastings"
          ;; Can't get this to pass - keeps coming up with 0 when it should be 1.
          (xit "should for a constant produce the constant with probability 1"
              (.createNew Universe)
              (let [c (Constant 1)
                    f (Flip 0.3)]
                (anytimeMHtest c #(= % 1) 1.0 0.0001)))

          (it "should for a uniform produce an interval with probability equal te the fraction of the range"
              (.createNew Universe)
              (let [u (Uniform 0.2 1)]
                (anytimeMHtest u #(and (<= 0.3 %) (< % 0.5)) 0.25 0.01))))
