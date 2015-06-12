(ns antonio.macros
  (:require [clojure.walk :refer [prewalk-replace]]
            [antonio.defaults :refer [*name* *universe*]]))


(defn gen-params
  "Given a list of parameters in the form '(% (dosomething  %)) returns a list of vectors of generated symbols to forms with the %n replaced by the symbol"
  [params]
  (map (fn [param]
         (let [sym (gensym)]
           [sym (prewalk-replace {'% sym} param)]))
       params))

(defmacro defprobability
  [figarofn & params]
  (let [gparams (gen-params params)]
    `(defn ~figarofn
       [~@(map first gparams)]
       (.probability ~(symbol (str figarofn "$/MODULE$"))
                     ~@(map second gparams)))))

(defmacro defapply*
  [figarofn & params]
  (let [gparams (gen-params params)]
    `(defn ~figarofn
       [~@(map first gparams)]
       (.apply ~(symbol (str figarofn "$/MODULE$"))
               ~@(map second gparams)
               *name* *universe*))))

(defn overload-defn
  [figarofn params]
  (let [gparams (gen-params params)]
    `([~@(map first gparams)]
      (.apply ~(symbol (str figarofn "$/MODULE$"))
              ~@(map second gparams)
              *name* *universe*))))

(defmacro defapply
  [figarofn & params]
  `(defn ~figarofn
     ~@(map (partial overload-defn figarofn) params)))

