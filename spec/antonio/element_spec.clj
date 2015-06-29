(ns antonio.element-spec
  (:require [speclj.core :refer :all]
            [antonio.macros :refer :all]
            [antonio.language :refer :all]
            [antonio.element :refer :all]
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
                (should= 5.0 (.value e))))

          (it "should convert to the correct string"
              (.createNew Universe)
              (should= "Constant(5.0)" (.toString (Constant 5)))))


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
                (should (= false (.value c)))
                (.set f1 true)
                (.generate c)
                (should= true (.value c))))

          (it "should return a cached result in"
              (with-local-vars [sum 0]
                (let [f (fn [b]
                          (var-set sum (inc @sum))
                          (Constant b))
                      c (CachingChain (Flip 0.5) f)]
                  (.get c true)
                  (.get c false)
                  (.get c true)
                  (should= 2 @sum))))

          #_( ;; Where is this Uniform with 3 parameters?
            it "should call the CPD when the cache is full"
              (.createNew Universe)
              (with-local-vars [sum 0]
                (let [f (fn [b]
                          (var-set sum (inc @sum))
                          (Constant b))
                      f1 (Uniform 0 1 2)
                      c (NonCachingChain f1 f)]))))

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
                (should= true (.value c))
                (.set f1 true)
                (.generate c)
                (should= false (.value c)))))

(describe "A chain with two parents caching"
          (it "should have value equal to the value of its function applied to its argument's value"
              (.createNew Universe)
              (let [f1 (Flip 0.7)
                    f2 (Flip 0.4)
                    f3 (Flip 0.9)
                    f4 (Flip 0.235)]
                (.set f1 false)
                (.set f2 true)
                (.set f3 false)
                (.set f4 true)
                (let [c (CachingChain f1 f2 (fn [b1 b2]
                                              (if (and b1 b2) f3 f4)))]
                  (.generate c)
                  (should= (.value c) true)
                  (.set f1 true)
                  (.generate c)
                  (should= false (.value c)))))

          (it "should evaluate the CPD only once for each input"
              (with-local-vars [sum 0]
                (letfn [(f [b1 b2]
                          (var-set sum (inc @sum))
                          (Constant (and b1 b2)))]
                  (let [f1 (Flip 0.5)
                        f2 (Flip 0.5)]
                    (.set f1 true)
                    (.set f2 true)
                    (let [c (CachingChain f1 f2 f)]
                      (.get c (tuple true true))
                      (.get c (tuple false false))
                      (.get c (tuple true true))
                      (should= 2 @sum))))) ))

(describe "An Apply with one argument"
          (it "should have value equal to its function applied to its argument"
              (.createNew Universe)
              (let [u (Uniform 0 2)
                    a (Apply u (fn [d] (inc d)))]
                (.set u 1.3)
                (.generate a)
                (should= 2.3 (.value a)))))

(describe "An Apply with two arguments"
          (it "should have value equal to its function applied to its arguments"
              (.createNew Universe)
              (let [u (Uniform 0 2)
                    v (Constant 1)
                    a (Apply u v (fn [d1 d2] (+ d1 d2 1)))]
                (.set u 1.3)
                (.set v 1)
                (.generate a)
                (should= 3.3 (.value a)))))

(describe "An Apply with three arguments"
          (it "should have value equal to its function applied to its arguments"
              (.createNew Universe)
              (let [u (Uniform 0 2)
                    v (Constant 1)
                    w (Select [[0.5 0]
                               [0.5 5]])
                    a (Apply u v w (fn [d1 d2 d3] (+ d1 d2 d3 1)))]
                (.set u 1.3)
                (.set v 1)
                (.set w 5)
                (.generate a)
                (should= 8.3 (.value a)))))

(describe "An Apply with four arguments"
          (it "should have value equal to its function applied to its arguments"
              (.createNew Universe)
              (let [u (Uniform 0 2)
                    v (Constant 1)
                    w (Select [[0.5 0]
                               [0.5 5]])
                    x (Constant -2)
                    a (Apply u v w x (fn [d1 d2 d3 d4] (+ d1 d2 d3 d4 1)))]
                (.set u 1.3)
                (.set v 1)
                (.set w 5)
                (.set x -2)
                (.generate a)
                (should= 6.3 (.value a)))))

(describe "An Apply with five arguments"
          (it "should have value equal to its function applied to its arguments"
              (.createNew Universe)
              (let [u (Uniform 0 2)
                    v (Constant 1)
                    w (Select [[0.5 0]
                               [0.5 5]])
                    x (Constant -2)
                    y (Constant 0.5)
                    a (Apply u v w x y (fn [d1 d2 d3 d4 d5] (+ d1 d2 d3 d4 d5 1)))]
                (.set u 1.3)
                (.set v 1)
                (.set w 5)
                (.set x -2)
                (.set y 0.5)
                (.generate a)
                (should= 6.8 (.value a)))))

(describe "An Inject"
          (it "should have value equal to the sequence of values of its arguments"
              (.createNew Universe)
              (let [u1 (Uniform 0 2)
                    u2 (Constant 1.5)
                    i (Inject [u1 u2])]
                (.set u1 1.3)
                (.set u2 1.5)
                (.generate i)
                (should= (value i) [1.3 1.5]))))

(describe "A Select with constant probabilities"
          (it "should have a value with its associated"
              (.createNew Universe)
              (sampleTest (Select [[0.2 1]
                                   [0.3 2]
                                   [0.5 3]])
                          #(> % 1)
                          0.8)))

(describe "A Select with variable probabilities"
          (it "should have a value with probability proportional to its associated probability element's value"
              (.createNew Universe)
              (let [c1 (Constant 0.2)
                    c2 (Constant 0.4)
                    d (Select [[c1 false]
                               [c2 true]])]
                (sampleTest d identity (/ 0.4 0.6)))))

(describe "A Dist with constant probabilities"
          (it "should have the value of an outcome with its associated probability"
              (.createNew Universe)
              (let [f (Flip 0.6)
                    c (Constant true)
                    d (Dist [[0.3 f]
                             [0.7 c]])]
                (.set f false)
                (sampleTest d identity 0.7))))

(describe "A Dist with variable probabilities"
          ;; This needs fixing.
          ;; Currently cannot get it to call the correct overload on Dist that
          ;; takes a seq of Elements. The seq of double overload is always called.
          (xit "should have tho value of an outcome with probability proportional to its associated probability element's value"
              (.createNew Universe)
              (let [f2 (Flip 0.5)
                    c1 (Constant 0.2)
                    c2 (Constant 0.4)
                    c3 (Constant true)
                    d (Dist [[c1 f2]
                             [c2 c3]])]
                (.set f2 false)
                (sampleTest d identity (/ 0.4 0.6)))))

(run-specs)
