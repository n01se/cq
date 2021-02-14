(ns net.n01se.cq
  (:refer-clojure :exclude [concat inc + - * / = not= < > <= >=])
  (:require [clojure.core :as clj])
  (:require [clojure.walk :refer [postwalk]])
  (:require [net.n01se.cq.core :refer
             [ffn

              get-path
              get-root
              get-value
              update-path
              reroot-path

              invoke
              invoke-value

              pipe
              span
              dot]]))

;; public API
(def | pipe)
(def & span)
(def . dot)

;; The list monad is additive, so it also supplies an mzero and mplus
;; It's mplus would be apply concat
(def hole
  (ffn ffn-hole [x] ())) ;; mzero for monadic vals

(def all
  (ffn ffn-all [x]
       (let [coll (get-value x)]
         (assert (coll? coll)
                 (str "`all` requires a seqable collection, not "
                      (type coll) ": " (pr-str coll)))
         (map-indexed (fn [i y] (update-path x conj i)) coll))))

(defn path [mf]
  (ffn ffn-path [x]
    (map get-path (invoke mf (reroot-path x)))))

(defn cq-first [mf]
  (ffn ffn-first [x]
    (take 1 (invoke-value mf x))))

(defmacro cq-let [[sym sym-mf] mf]
  `(ffn ~'ffn-cq-let [x#]
     (let [value# (invoke ~sym-mf x#)
           ~sym (constantly value#)
           result# (invoke ~mf x#)]
       result#)))

(defmacro cq-letfn [fnforms mf]
  `(ffn ~'ffn-cq-letfn [x#]
        (letfn ~fnforms (invoke ~mf x#))))

(defmacro cq-letlift [fn-vec mf]
  `(ffn ~'ffn-cq-letlift [x#]
        (let [~@(mapcat (fn [fn-sym] [fn-sym `(lift ~fn-sym)])
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

(defn cq-get [mf]
  (ffn ffn-get [x]
    (let [path-elems (invoke-value mf x)]
      (map #(update-path x conj %) path-elems))))

(defn modify [path-mf value-mf]
  (ffn ffn-modify [x]
    (list
     (reduce
      (fn [acc value-path]
        ;; some versions before jq-1.6 use `last` instead of `first`:
        (let [deep-value (get-value (first (invoke value-mf value-path)))
              path (get-path value-path)]
          (if (empty? path)
            deep-value ;; c'mon, Clojure!
            (assoc-in acc path deep-value))))
      (get-value x)
      (invoke path-mf (reroot-path x))))))

(defn select [mf]
  (ffn ffn-select [x]
       (mapcat #(when % (list x))
               (invoke-value mf x))))

;; EXPERIMENTAL dynamically rooted paths
(def rooted
  (ffn ffn-rooted [x]
       (list (reroot-path x))))

(def rooted-path
  (ffn ffn-rooted-path [x]
       (list (get-path x))))

(defn rooted-reset [mf]
  (ffn ffn-rooted-reset [x]
    (list
     (let [deep-value (get-value (first (invoke mf x)))]
       (assoc-in (get-root x) (get-path x) deep-value)))))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(defn lift [f]
  (fn lifted [& mf]
    (ffn ffn-lift [x]
      (map #(apply f %)
           (cartesian-product (map #(invoke-value % x) mf))))))

(defn cq [mf]
  (invoke-value mf nil))

(defn cq1 [mf]
  (first (invoke-value mf nil)))

;; lifted functions

(def concat (lift clj/concat))
(def inc (lift clj/inc))
(def + (lift clj/+))
(def - (lift clj/-))
(def * (lift clj/*))
(def / (lift clj//))
(def = (lift clj/=))
(def not= (lift clj/not=))
(def > (lift clj/>))
(def < (lift clj/<))
(def >= (lift clj/>=))
(def <= (lift clj/<=))
