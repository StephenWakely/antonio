(ns antonio.defaults
  (:import (com.cra.figaro.language Universe$ Name$)))

(def ^:dynamic *universe* (fn [] (.universe Universe$/MODULE$)))
(def ^:dynamic *name* (.default Name$/MODULE$))
