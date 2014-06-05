(ns tree-transforms.core
  (:import java.lang.Long
           java.lang.String
           clojure.lang.PersistentVector))

(def Vector PersistentVector)

;; traverse thing what how

;; linear traversal
(def aline [1 "foo"])

(defmulti  ltrav class)
(defmethod ltrav Long   [x] (inc x))
(defmethod ltrav String [x] (str x "b"))
(defmethod ltrav Vector [x] (mapv ltrav x))

(assert (= [2 "foob"]
           (ltrav aline)))


