(ns net.n01se.cq
  (:refer-clojure :exclude [concat inc + - * / = not= < > <= >=])
  (:require [clojure.core :as clj])
  (:require [clojure.walk :refer [postwalk]])
  (:require [net.n01se.cq.core :as core :refer
             [mfn
              def-mfc

              get-path
              get-root
              get-value
              as-value-path
              update-path
              reroot-path

              invoke
              invoke-value]]))

;; public API
(def emit core/emit)
(def & core/&)

(def-mfc | [& mfs] [x] ;; pipe: variatic monoid-plus over monadic fns
  (reduce (fn [mx mf] (mapcat #(invoke mf %) mx))
          (list x)
          mfs))

(def .
  (mfn mfn-dot {:mf-expr '.} [x]
       (list x)))

;; The list monad is additive, so it also supplies an mzero and mplus
;; It's mplus would be apply concat
(def hole ;; consider encouraging use of `(&)` instead of `hole`?
  (mfn mfn-hole {:mf-expr 'hole} [x] ()))

(def all
  (mfn mfn-all {:mf-expr 'all} [x]
       (let [coll (get-value x)]
         (assert (coll? coll)
                 (str "`all` requires a seqable collection, not "
                      (type coll) ": " (pr-str coll)))
         (map-indexed (fn [i y] (update-path x conj i)) coll))))

(def-mfc path [mf] [x]
  (map get-path (invoke mf (reroot-path x))))

(def-mfc cq-first [mf] [x]
  (take 1 (invoke-value mf x)))

(defmacro cq-let [[sym sym-mf] mf]
  `(mfn ~'mfn-cq-let {:mfc-expr '~(list 'cq-let [sym sym-mf] mf)} [x#]
        (let [value# (invoke ~sym-mf x#)
              ~sym (constantly value#)
              result# (invoke ~mf x#)]
          result#)))

(defmacro cq-letfn [fnforms mf]
  `(mfn ~'mfn-cq-letfn {:mfc-expr '~(list 'cq-letfn fnforms mf)} [x#]
        (letfn ~fnforms (invoke ~mf x#))))

(defmacro cq-letlift [fn-vec mf]
  `(mfn ~'mfn-cq-letlift {:mfc-expr '~(list 'cq-letlift fn-vec mf)} [x#]
        (let [~@(mapcat (fn [fn-sym]
                          [fn-sym `(lift ~fn-sym
                                         {:sym '~(symbol "lifted"
                                                         (name fn-sym))})])
                        fn-vec)]
          (invoke ~mf x#))))


(defn var-name [the-var]
  (symbol (second (re-matches #"#'(.*)" (str the-var)))))

(defmacro with-refer-all
  "Take a vector of namespaces (or ns aliases) and make all their publics
  available without qualification to the body param."
  [ns-name-vector & body]
  ;; Non-macro vars are provided in an outer `let`, allowing normal shadowing in
  ;; the body. Since macros cannot be bound via `let`, the body is walked
  ;; looking for macro invocations, and the macro name is replaced with the
  ;; fully-qualified form.
  (let [aliases (ns-aliases *ns*)
        ns-list (map #(or (get aliases %) (the-ns %)) ns-name-vector)
        var-map (apply merge-with (fn [a b] a) (map ns-publics ns-list))
        {macros true, others nil} (group-by #(:macro (meta (val %))) var-map)
        macro-map (into {} macros)
        others-map (dissoc (into {} others) '.)]
    `(let [~@(->> (tree-seq coll? seq body)
                  (mapcat (fn [form]
                            (when-let [[sym v] (find others-map form)]
                              [sym (var-name v)]))))]
       ~@(postwalk
          (fn [form]
            (if-let [[sym v]
                     (and (seq? form) (find macro-map (first form)))]
              (list* (var-name v) (rest form))
              form))
          body))))

(defmacro with-tracing [& body]
  `(binding [core/*tracing* 1]
     ~@body))

(def-mfc cq-get [mf] [x]
  (let [path-elems (invoke-value mf x)]
    (map #(update-path x conj %) path-elems)))

(def-mfc modify [path-mf value-mf] [x]
  (list
   (reduce
    (fn [acc value-path]
      ;; some versions before jq-1.6 use `last` instead of `first`:
      (let [deep-value (get-value (clj/first (invoke value-mf value-path)))
            path (get-path value-path)]
        (if (empty? path)
          deep-value ;; c'mon, Clojure!
          (assoc-in acc path deep-value))))
    (get-value x)
    (invoke path-mf (reroot-path x)))))

(def-mfc select [mf] [x]
  (mapcat #(when % (list x))
          (invoke-value mf x)))

;; EXPERIMENTAL dynamically rooted paths
(def rooted
  (mfn mfn-rooted {:mf-expr 'rooted} [x]
       (list (reroot-path x))))

(def rooted-path
  (mfn mfn-rooted-path {:mf-expr 'rooted-path} [x]
       (list (get-path x))))

(def-mfc rooted-reset [mf] [x]
  (list
   (let [deep-value (get-value (clj/first (invoke mf x)))]
     (assoc-in (get-root x) (get-path x) deep-value))))

(defn cartesian-product ;; TODO move to impl
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(defn lift [f & [{:keys [sym]}]]
  (fn lifted [& mf]
    (mfn mfn-lift {:mfc-expr (cons sym mf)} [x]
         (map #(apply f %)
              (cartesian-product (map #(invoke-value % x) mf))))))

;; Fully-short-circuiting `and` -- perhaps put in jq-compatibility namespace?
#_
(def-mfc cq-and [mfa mfb] [x]
  (let [*bs (delay (invoke-value mfb x))]
    (mapcat
     (fn [a]
       (if-not a
         (list a) ;; false
         @*bs))
     (invoke-value mfa x))))

(defn cq [mf]
  (invoke-value mf nil))

(defn cq1 [mf]
  (first (invoke-value mf nil)))

;; lifted functions

(defmacro def-lift [sym clj-fn]
  `(def ~sym (lift ~clj-fn {:sym '~sym})))

(def-lift concat clj/concat)
(def-lift inc clj/inc)
(def-lift + clj/+)
(def-lift - clj/-)
(def-lift * clj/*)
(def-lift / clj//)
(def-lift = clj/=)
(def-lift not= clj/not=)
(def-lift > clj/>)
(def-lift < clj/<)
(def-lift >= clj/>=)
(def-lift <= clj/<=)

;; These are not like the jq functions; they return all combinatorics because we
;; think that makes sense. They do not short-circuit:
(def-lift cq-and (fn [a b] (and a b)))
(def-lift cq-or  (fn [a b] (or  a b)))
