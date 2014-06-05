(ns tree-transforms.core)

;;; helpers
(defn append-kw [k1 k2]
  (str (name k1) (name k2)))

;; traverse thing what how

;; linear traversal
(def aline [1 :a "foo"])

(defmulti ltrav class)
(defmethod ltrav java.lang.Long [x] (inc x))
(defmethod ltrav clojure.lang.Keyword [x] (append-kw x :b))
(defmethod ltrav java.lang.String [x] (str x "b"))
(defmethod ltrav clojure.lang.PersistentVector [x] (vec (map ltrav x)))

(assert (= [2 "ab" "foob"]
           (ltrav aline)))
