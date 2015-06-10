(ns figaro-clj.defaults
  (:import (com.cra.figaro.language Universe$ Name$)))

(def ^:dynamic *universe* (.universe Universe$/MODULE$))
(def ^:dynamic *name* (.default Name$/MODULE$))
