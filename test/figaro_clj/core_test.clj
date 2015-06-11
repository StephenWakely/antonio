(ns figaro-clj.core-test
  (:require [clojure.test :refer :all]
            [figaro-clj.language :refer :all]
            [figaro-clj.elements :refer :all]
            [figaro-clj.element :refer :all]
            [figaro-clj.algorithms :refer :all]))


;; http://gettingclojure.wikidot.com/cookbook:numbers
(defn float=
  ([x y] (float= x y 0.00001))
  ([x y epsilon]
     (let [scale (if (or (zero? x) (zero? y)) 1 (Math/abs x))]
       (<= (Math/abs (- x y)) (* scale epsilon)))) )

(deftest weather-test
  []
  (let [sunnyToday (Flip 0.2)
        greetingToday (If sunnyToday
                           (Select [[0.6 "Hello"]
                                    [0.4 "Howdy"]])
                           (Select [[0.2 "Hello"]
                                    [0.8 "Oh no"]]))
        sunnyTomorrow (If sunnyToday
                           (Flip 0.8)
                           (Flip 0.05))
        greetingTomorrow (If sunnyTomorrow
                              (Select [[0.5 "Hello"]
                                       [0.5 "Howdy"]])
                              (Select [[0.1 "Hello"]
                                       [0.9 "Oh no"]]))]
    (is (float= (VariableElimination greetingToday "Hello") 0.28 0.1))
    (observe greetingToday "Hello")
    (is (float= (VariableElimination sunnyToday true) 0.42 0.1))
    (is (float= (VariableElimination greetingTomorrow "Hello") 0.24 0.1))))

(deftest mood-test
  []
  (let [goodMood (Dist [[0.2 (Flip 0.6)]
                        [0.8 (Flip 0.2)]])]
    (is (float= (VariableElimination goodMood true) 0.28 0.001))))

(defn monthQuality
  []
  (let [sunnyDaysInMonth (Binomial 30 0.2)]
    (Apply sunnyDaysInMonth #(cond (> % 10) "good"
                                   (> % 5) "average"
                                   :else "poor"))))

(deftest apply-test
  []
  (is (float= (VariableElimination (monthQuality) "good") 0.025616 0.01)))

(deftest chain-test
  []
  (let [sunnyToday (Flip 0.2)
        goodMood (Chain (monthQuality) sunnyToday
                        (fn [quality sunny]
                          (if sunny
                            (cond (= quality "good") (Flip 0.9)
                                  (= quality "average") (Flip 0.7)
                                  :else (Flip 0.4))
                            (cond (= quality "good") (Flip 0.6)
                                  (= quality "average") (Flip 0.3)
                                  :else (Flip 0.05)))))]
    ;; TODO: Check - text says this should be 0.2896316752495942
    (is (float= (VariableElimination goodMood true) 0.276 0.001))))


(deftest condition-test
  []
  (let [sunnyDaysInMonth (Binomial 30 0.2)
        monthQuality (Apply sunnyDaysInMonth
                            #(cond (> % 10) "good"
                                   (> % 5) "average"
                                   :else "poor"))
        goodMood (Chain monthQuality
                        #(cond (= % "good") (Flip 0.9)
                               (= % "average") (Flip 0.6)
                               :else (Flip 0.1)))]
    (is (float= (VariableElimination goodMood true) 0.393928 0.001))
    (setCondition sunnyDaysInMonth #(> % 8))
    (is (float= (VariableElimination goodMood true) 0.65973 0.001))))
