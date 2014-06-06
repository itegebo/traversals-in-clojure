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

(defn case->defmethod [n ps case]
  (let [[dv body] case]
    `(defmethod ~n ~dv ~ps ~body)))

;; I deliberately put the dispatch value over the cases column
;; also, wrapping clauses in a vec was because I didn't easily think
;; of a func for pairing up every 2 elements...well, okay:
(defn pairup [l ps]
  (if-let [[a b & r] l]
    (pairup r (conj ps [a b]))
    ps))

(def defrec-example '(defrec ltrav3 [x]
                       class
                       [Long   (inc x)]
                       [String (str x "b")]
                       [Vector (mapv ltrav3 x)]))

(assert (= (let [[_ n ps _ & cases] defrec-example]
             (case->defmethod n ps (first cases)))
           '(clojure.core/defmethod ltrav3 Long [x] (inc x))))

(defmacro defrec [n ps dfn & cases]
  (let [defmethods (map #(case->defmethod n ps %) cases)]
    `(do
       (defmulti ~n ~dfn)
       ~@defmethods)))

;; TODO Add test for defrec

(defrec ltrav3 [x]
  class
  [Long   (inc x)]
  [String (str x "b")]
  [Vector (mapv ltrav3 x)])

(assert (= (ltrav3 aline)
           [2 "foob"]))

(assert (= (ltrav3 [[1] [[["foo"]]]])
           [[2] [[["foob"]]]]))

;;; Observations about what a "defrec" would need to do:
;; specify a :default multimethod for handling invalid input
;; could identity recursive vs base cases

;;; How would defrec specify ltrav2?
;; it might be too simple, one wouldn't implement that that way

;;; Quickly, here's the version with the explicit vecs around cases

(def defrec-example2 '(defrec ltrav3 [x]
                        class
                        Long   (inc x)
                        String (str x "b")
                        Vector (mapv ltrav3 x)))

(defmacro defrec2 [n ps dfn & cases]
  (let [defmethods (->> (pairup cases [])
                        (map #(case->defmethod n ps %)))]
    `(do
       (defmulti ~n ~dfn)
       ~@defmethods)))

(defrec2 ltrav3-with-defrec2 [x]
  class
  Long   (inc x)
  String (str x "b")
  Vector (mapv ltrav3 x))

;;; Okay, that's nice.  Let's do a vec/map/basecase example

;; TODO
