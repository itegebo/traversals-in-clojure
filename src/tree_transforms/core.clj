(ns tree-transforms.core)

;; traverse thing what how

;; linear traversal
(def aline [1 "foo"])

(defmulti ltrav class)
(defmethod ltrav java.lang.Long [x] (inc x))
(defmethod ltrav java.lang.String [x] (str x "b"))
(defmethod ltrav clojure.lang.PersistentVector [x] (vec (map ltrav x)))

(assert (= [2 "foob"]
           (ltrav aline)))
