(ns figaro-clj.elements
  (:require [clojure.reflect :as r]
            [clojure.pprint :as pp]
            [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala]
            [figaro-clj.macros :refer [defapply]]
            [figaro-clj.defaults :refer [*name* *universe*]])
  (:import (com.cra.figaro.library.atomic.continuous Normal$ Uniform$)
           (com.cra.figaro.library.atomic.discrete Binomial$)))

(defapply Normal (double %) (double %))
(defapply Uniform (double %) (double %))

(defapply Binomial % (double %))
