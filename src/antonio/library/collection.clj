(ns antonio.library.collection
  (:require [t6.from-scala.core :refer [$ $$] :as $]
            [t6.from-scala.utils :as scala])
  (:import (com.cra.figaro.library.collection Container$)))

(defn Container
  ([elements]
   (.apply Container$/MODULE$ (apply scala/immutable-list elements)))
  ([numItems generator]
   (.apply Container$/MODULE$ numItems ($/fn [i] #(generator 1)))))

