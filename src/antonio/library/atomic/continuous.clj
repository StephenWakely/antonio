(ns antonio.library.atomic.continuous
  (:require [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala]
            [antonio.macros :refer [defapply]]
            [antonio.defaults :refer [*name* *universe*]])
  (:import (com.cra.figaro.library.atomic.continuous Normal$ Uniform$)))

(defapply Normal [(double %) (double %)])
(defapply Uniform [(double %) (double %)])


