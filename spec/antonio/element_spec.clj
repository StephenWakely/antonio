(ns antonio.element-spec
  (:require [speclj.core :refer :all]
            [antonio.language :refer :all]
            [antonio.library.atomic.continuous :refer :all]
            [antonio.equals :refer :all]))


(defn sampleTest
  [targetElem predicate prob]
  (let [numTrials 100000
        tolerance 0.01
        successes (reduce (fn [s _]
                            (.generate targetElem)
                            (if (predicate (.value targetElem)) (inc s) s))
                          0 (range numTrials))]
    (should= (/ successes numTrials) prob tolerance)))

(describe "A Constant"

          (it "should have value equal to the given constant"
              (.createNew Universe)
              (let [e (Constant 5)]
                (.generate e)
                (should= (.value e) 5)))

          (it "should convert to the correct string"
              (.createNew Universe)
              (should= (.toString (Constant 5))
                         "Constant(5)")))


(describe "A Flip with constant weight"

          (it "should have value true with probability equal to the weight"
              (.createNew Universe)
              (sampleTest (Flip 0.7) (fn [v] (= v true)) 0.7))

          (it "should convert to the correct string"
              (.createNew Universe)
              (should= (.toString (Flip 0.7))
                         "Flip(0.7)")))

(describe "A Flip with a random weight"

          (it "should have value true with probability equal to the expectation of the weight"
              (.createNew Universe)
              (let [u (Uniform 0 0.5)
                    f (Flip u)]
                (.set u 0.25)
                (sampleTest f #(= % true) 0.25)))

          (it "should convert to the correct string"
              (.createNew Universe)
              (let [u (Uniform 0 0.5)]
                (should= (.toString (Flip u)) (str "Flip(" u ")")))))


(describe "A chain when called"

          (it "should have value equal to the value of its function applied to its argument's value"
              (.createNew Universe)
              (let [f1 (Flip 0.7)
                    f2 (Flip 0.4)
                    f3 (Flip 0.9)
                    c (NonCachingChain f1 (fn [b] (if b f2 f3)))]
                (.set f1 false)
                (.set f2 true)
                (.set f3 false)
                (.generate c)
                (should (= (.value c) false))
                (.set f1 true)
                (.generate c)
                (should= (.value c) true)))

          (it "should return a cached result in"
              (with-local-vars [sum 0]
                (let [f (fn [b]
                          (var-set sum (inc @sum))
                          (Constant b))
                      c (CachingChain (Flip 0.5) f)]
                  (.get c true)
                  (.get c false)
                  (.get c true)
                  (should= @sum 2))))

          #_( ;; Where is this Uniform with 3 parameters?
            it "should call the CPD when the cache is full"
              (.createNew Universe)
              (with-local-vars [sum 0]
                (let [f (fn [b]
                          (var-set sum (inc @sum))
                          (Constant b))
                      f1 (Uniform 0 1 2)
                      c (NonCachingChain f1 f)]))))

(describe "A chain managing the context"
          #_( ;; Private vars..
             it "should store new elements in the correct subContext"
              (.createNew Universe)
              (let [c (Chain (Flip 0.5)
                             (fn [b] (if b (Constant 0) (Constant 1))))]
                (.get c true)
                (.get c false)
                (should= (-> c (.myMappedContextContents true) .size) 1))))

(describe "A chain with two parents non-caching"
          (it "should have value equal to the value of its function applied to its argument's values"
              (.createNew Universe)
              (let [f1 (Flip 0.4)
                    f2 (Flip 0.7)
                    f3 (Flip 0.9)
                    f4 (Flip 0.235)
                    c (NonCachingChain f1 f2 (fn [b1 b2] (if (and b1 b2) f3 f4)))]
                (.set f1 false)
                (.set f2 true)
                (.set f3 false)
                (.set f4 true)
                (.generate c)
                (should= (.value c) true))))

(run-specs)
