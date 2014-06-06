(ns tree-transforms.core
  (:import java.lang.Long
           java.lang.String
           clojure.lang.PersistentVector))

(def Vector PersistentVector)

;; traverse thing what how

;; linear traversal
(def aline [1 "foo"]) ; Vector of ( Long | String | Vector )

(defmulti  ltrav class)
(defmethod ltrav Long   [x] (inc x))
(defmethod ltrav String [x] (str x "b"))
(defmethod ltrav Vector [x] (mapv ltrav x))

(assert (= [2 "foob"]
           (ltrav aline)))

;; whoops, not actually "linear"
(assert (= [[2] [[["foob"]]]]
           (ltrav [[1] [[["foo"]]]])))

;; maybe we don't vector nesting
(defmulti  ltrav2d class) ; ltrav take 2, dispatcher
(defmethod ltrav2d Long   [x] (inc x))
(defmethod ltrav2d String [x] (str x "b"))
(defn ltrav2 [x]
  (try
    (mapv ltrav2d x)
    ;; TODO Catch only the following case
    ;; No method in multimethod 'ltrav2d' for dispatch value:
    ;; class clojure.lang.PersistentVector
    (catch java.lang.IllegalArgumentException e
      (if (re-matches #".*ltrav2d.*" (.getMessage e))
        (do (println "we don't accept nesting")
            nil)
        (throw e)))))

(assert (= [2 "foob"]
           (ltrav2 aline)))

(assert (not= [[2] [[["foob"]]]]
              (ltrav2 [[1] [[["foo"]]]])))

;;; Can we rephrase the nested vec version to reveal the structure
;;; like a pattern match would?

(defmulti  ltrav3 class)
(defmethod ltrav3 Long   [x] (inc x))
(defmethod ltrav3 String [x] (str x "b"))
(defmethod ltrav3 Vector [x] (mapv ltrav3 x))

(defn clause->defmethod [n ps clause]
  (let [[dv body] clause]
    `(defmethod ~n ~dv ~ps ~body)))

(assert (= (let [[_ n _ ps & clauses] defrec-example]
             (clause->defmethod n ps (first clauses)))
           '(clojure.core/defmethod ltrav3 Long [x] (inc x))))

(defmacro defrec [n dfn ps & clauses]
  (let [defmethods (map #(clause->defmethod n ps %) clauses)]
    `(do
       (defmulti ~n ~dfn)
       ~@defmethods)))

(def defrec-example '(defrec ltrav3 class
                       [x]
                       [Long   (inc x)]
                       [String (str x "b")]
                       [Vector (mapv ltrav3 x)])) 

;; TODO Add test for defrec

(defrec ltrav3 class
  [x]
  [Long   (inc x)]
  [String (str x "b")]
  [Vector (mapv ltrav3 x)])

(assert (= [2 "foob"]
           (ltrav3 aline)))

(assert (= [[2] [[["foob"]]]]
           (ltrav3 [[1] [[["foo"]]]])))
