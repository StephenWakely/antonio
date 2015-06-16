(ns antonio.equals)

(defn abs
  [val] (if (< val 0) (* val -1) val))

;; http://gettingclojure.wikidot.com/cookbook:numbers
(defn float=
  ([x y] (float= x y 0.00001))
  ([x y epsilon]
     (let [scale (if (or (zero? x) (zero? y)) 1 (abs x))]
       (<= (abs (- x y)) (* scale epsilon)))) )

