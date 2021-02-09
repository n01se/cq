(ns net.n01se.cq
  (:require [clojure.walk :refer [postwalk]]))

;; ValuePath
;; This is a bit like a writer monad with root as the value and path as the log?
(defprotocol IValuePath
  (get-value [_] "Return the resolved, normal Clojure value")
  (get-root [_] "Return the root, normal Clojure value. Throw if not ValuePath")
  (get-path [_] "Return the path vector")
  (update-path* [x f args]
    "Return ValuePath with same root as x and path of (apply f old-path args).
     If x is not a ValuePath, return a ValuePath rooted at x with value x"))

(deftype ValuePath [root-value path]
  Object
  (equals [a b] (and (= (.root-value a) (.root-value b))
                     (= (.path a) (.path b))))

  IValuePath
  (get-value [x] (get-in root-value path))
  (get-root [x] root-value)
  (get-path [x] path)
  (update-path* [x f args] (ValuePath. root-value (apply f path args))))

(def DefaultIValuePath
  {:get-value identity
   :get-root #(throw (ex-info (str "Invalid path expression: " %) {:path %}))
   :get-path #(throw (ex-info (str "Invalid path expression: " %) {:path %}))
   :update-path* (fn [x f args] (ValuePath. x (apply f [] args)))})

(extend nil     IValuePath DefaultIValuePath)
(extend Object  IValuePath DefaultIValuePath)

(defn update-path [x f & args]
  (update-path* x f args))

(defn as-value-path [x]
  (update-path x identity))

(defn reroot-path [x]
  (as-value-path (get-value x)))


(defn cartesian-product
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(declare span)
(declare invoke-value)

(defn invoke [mf x]
  (cond
    (number? mf) (list mf)
    (string? mf) (list mf)
    (vector? mf) (list (vec (invoke-value (apply span mf) x)))
    (fn? mf) (mf x)
    (var? mf) (mf x)
    (nil? mf) nil ;; Complain? jq doesn't
    :else (throw (ex-info (str "Can't invoke " (pr-str mf)) {}))))

(defn invoke-value [mf x]
  (map get-value (invoke mf x)))

(defn cq [mf]
  (invoke-value mf nil))

(defn cq1 [mf]
  (first (invoke-value mf nil)))

(def tracing false)
(def ^:dynamic *ffn-depth* 1)
(defn trace [& args] (apply println (apply str (repeat *ffn-depth* " ")) args))

(defmacro ffn
  "Exactly like (fn name [args] ...), but asserts the return value is
  sequential."
  [name-sym argv & body]
  (assert (symbol? name-sym))
  (assert (vector? argv))
  (assert (= 1 (count argv)))
  `(fn ~name-sym ~argv
     (when tracing (trace "Piping" ~(first argv) "into" '~name-sym))
     (let [result# (binding [*ffn-depth* (inc *ffn-depth*)] (do ~@body))]
       (when tracing (trace "Return" result# "from" '~name-sym))
       (assert (sequential? result#)
               (str ~(str "Non-list returned by " name-sym " for args: ")
                    (pr-str ~(first argv))))
       result#)))

;; There is a list monad at the base of our operations:
;; ...where "unit" is list and "bind" is mapcat except the arguments are reversed

(defn pipe [& mfs] ;; variatic monoid-plus over monadic fns
  (ffn ffn-pipe [x]
       (reduce (fn [mx mf] (mapcat #(invoke mf %) mx))
               (list x)
               mfs)))

;; monoid-plus over monadic vals obtained by invoking each mf with x
(defn span [& mfs] ;; rename: cq-mapcat ?
  (ffn ffn-span [x]
       (mapcat #(invoke % x) mfs)))

;; The list monad is additive, so it also supplies an mzero and mplus
;; It's mplus would be apply concat
(def hole
  (ffn ffn-hole [x] ())) ;; mzero for monadic vals

(def dot
  (ffn ffn-dot [x]
       (list (as-value-path x))))

(def . dot)

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

(defn lift [f]
  (fn lifted [& mf]
    (ffn ffn-lift [x]
      (map #(apply f %)
           (cartesian-product (map #(invoke-value % x) mf))))))

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
