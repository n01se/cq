(ns net.n01se.cq.internal
  (:require [clojure.core :as clj]
            [clojure.walk :refer [postwalk]]))

(binding [*ns* *ns*] (in-ns 'net.n01se.cq))

;;=== utility functions and macros

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(defn cartesian-product-rev
  [colls]
  (map reverse (cartesian-product (reverse colls))))

(defmacro ex-assert [expr & [msg]]
  `(when-not ~expr
     (throw (ex-info ~(or msg (pr-str expr)) {:expr '~expr}))))

(defn var-name [the-var]
  (symbol (second (re-matches #"#'(.*)" (str the-var)))))

(defmacro ^:publish with-refer-all
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
        others-map (dissoc (into {} others) '. '&)]
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

(defmacro ^:publish do
  "Convenience macro for `(with-refer-all [cq] (eval ~@body))"
  [& body]
  `(with-refer-all [net.n01se.cq] (cq-eval ~@body)))


;;=== list monad support (emit, tracing, mfc, and def-mfc)

;; There is a list monad at the base of our operations: ...where "unit" is list
;; (aka `.`) and "bind" is mapcat except the arguments are reversed

(declare emit)

(defn emit-meta [{:keys [mfc-expr mf-expr]}]
  (if-let [[f & args] mfc-expr]
    (cons f (map emit args))
    mf-expr))

(defn ^:publish emit [mf]
  (cond
    (vector? mf) (mapv emit mf)
    :else (or (emit-meta (meta mf)) mf)))

(def ^:dynamic *tracing* false)
(def ^:dynamic *mfn-depth* 1)
(defn trace [& args]
  (when *tracing*
    (apply println (apply str (repeat *mfn-depth* " ")) args)))

(defmacro ^:publish with-tracing [& body]
  `(binding [*tracing* 1]
     ~@body))

(def heavy-runtime true) ;; not sure how to parameterize this
(if heavy-runtime
  (defmacro mfn
    "Exactly like (fn name [args] ...), but asserts the return value is
  sequential. Returns a list-monadic function."
    [name-sym attr-map argv & body]
    (assert (symbol? name-sym))
    (assert (vector? argv))
    (assert (= 1 (count argv)))
    (assert (map? attr-map))
    `(let [emitted# (emit-meta ~attr-map)]
       (with-meta
         (fn ~name-sym ~argv
           (trace "Pipe" (pr-str ~(first argv)) "into" (pr-str emitted#))
           (let [result# (binding [*mfn-depth* (inc *mfn-depth*)] (do ~@body))]
             (trace "Return" (pr-str result#) "from" (pr-str emitted#))
             (assert (sequential? result#)
                     (str "Non-list returned by "
                          emitted#
                          " for args: "
                          (pr-str ~(first argv))))
             result#))
         ~attr-map)))
  (defmacro mfn [name-sym attr-map argv & body]
    (list* 'fn argv body)))

(defmacro def-mfc
  "Define a named list-monadic-function constructor. When called, the ctor
  function retuns a monadic function that closes over the ctors args and has
  metadata naming the ctor and the ctor args, for use in tracing and debugging."
  [sym arg-syms [this-sym :as this-args] & body]
  (assert (symbol? sym))
  (assert (vector? arg-syms))
  (assert (vector? this-args))
  `(defn ~(with-meta (symbol (str "cq-" sym))
            {:publish (str sym)})
     [& args#]
     (let [~arg-syms args#]
       (mfn ~(symbol (str "mfn-" sym)) {:mfc-expr (cons '~sym args#)} [~this-sym]
            ~@body))))


;;=== Navigation

(declare invoke invoke-value cq-eval1 cq-eval)

(defprotocol INavigation
  (navigate [_] "return value by applying navigation")
  (chart [_] "return description of navigation")
  (modify* [_ base f] "rebase nav onto base and update the new target with f"))

(def DefaultINavigation
  {:navigate identity
   :chart (constantly [])
   :modify* (fn [me base f] (f base))})

(extend nil     INavigation DefaultINavigation)
(extend Object  INavigation DefaultINavigation)

(defprotocol ILens
  (lens-get [_ obj] "get me from obj")
  (lens-put [_ obj newval] "replace me with newval in obj")
  (lens-chart [_] "describe me as a path elem"))

(defn nav-lens [parent lens]
  (reify
    INavigation
    (navigate [_] (lens-get lens (navigate parent)))
    (chart [_] (conj (chart parent) (lens-chart lens)))
    (modify* [_ base f]
      (modify* parent base #(lens-put lens % (f (lens-get lens %)))))))

(defn nav-get [parent idx]
  (let [idx (navigate idx)]
    (nav-lens parent
              (reify
                ILens
                (lens-get [_ obj] (get obj idx))
                (lens-put [_ obj newval] (assoc obj idx newval))
                (lens-chart [_] idx)))))

(defn nav-slice [parent start-idx end-idx]
  (let [start-idx (navigate start-idx)
        end-idx (navigate end-idx)]
    (nav-lens parent
              (reify
                ILens
                (lens-get [_ obj]
                  (->> obj
                       (drop start-idx)
                       (take (- end-idx start-idx))
                       vec))
                (lens-put [_ obj newval]
                  (ex-assert (sequential? newval)
                             (str "modify on slice must return sequential, not "
                                  (type newval) ": " (pr-str newval)))
                  (concat (take start-idx obj)
                          newval
                          (drop (max start-idx end-idx) obj)))
                (lens-chart [_] {:start start-idx, :end end-idx})))))

(defn ^:publish lift-nav-aware [f & [{:keys [sym]}]]
  (fn nav-lifted [& mfs]
    (mfn mfn-nav-lift {:mfc-expr (cons sym mfs)} [x]
         (map #(apply f %)
              (cartesian-product (map #(invoke % x) mfs))))))

(def ^:publish pick (lift-nav-aware nav-get))
(def ^:publish slice (lift-nav-aware nav-slice))

;; TODO clearly document first-behavior of value-mf
(def-mfc modify [path-mf value-mf] [x]
  (let [xval (navigate x)]
    (list
     (reduce
      (fn [acc nav]
        ;; some versions before jq-1.6 use `last` instead of eval1's `first`:
        (modify* nav acc #(cq-eval1 % value-mf)))
      xval
      (invoke path-mf xval)))))

(def-mfc assign [nav-mf value-mf] [x]
  (let [xval (navigate x)
        navs (invoke nav-mf xval)
        values (cq-eval xval value-mf)]
    (map (fn [value]
           (reduce (fn [xval nav]
                     (modify* nav xval (constantly value)))
             xval
             navs))
         values)))

;;=== invoke and eval

;; TODO: get rid of this in favor of cq-eval everywhere
(defn invoke-value [mf x]
  (map navigate (invoke mf x)))

(defn ^{:publish 'eval} cq-eval
  ([mf] (invoke-value mf nil))
  ([input mf] (invoke-value mf input)))

(defn ^{:publish 'eval1} cq-eval1 ;; TODO: throw if more than one in return list?
  ([mf] (first (invoke-value mf nil)))
  ([input mf] (first (invoke-value mf input))))

(declare &)

(defn ^:publish lift [f & [{:keys [sym]}]]
  (fn lifted [& mfs]
    (mfn mfn-lift {:mfc-expr (cons sym mfs)} [x]
         (map #(apply f %)
              (cartesian-product (map #(invoke-value % x) mfs))))))

;; invoke lifts Clojure constants into monadic functions
(defn invoke [mf x]
  (cond
    (number? mf) (list mf)
    (string? mf) (list mf)
    (boolean? mf) (list mf)
    (keyword? mf) (list mf)
    (vector? mf) ((apply (lift vector {:sym 'vector}) mf) x)
    (map? mf) ((apply (lift array-map {:sym 'array-map}) (apply concat mf)) x)
    (fn? mf) (mf x)
    (var? mf) (mf x)
    (nil? mf) (list nil) ;; Complain? jq doesn't
    :else (throw (ex-info (str "Can't invoke " (pr-str mf)) {}))))

(def-mfc collect [src-mf] [x]
  (list (cq-eval x src-mf)))

(def-mfc collect-into [target-mf src-mf] [x]
  (map #(into % (cq-eval x src-mf)) (cq-eval x target-mf)))

;;=== cq library of monadic functions and their constructors

;; monoid-plus over monadic vals obtained by invoking each mf with x
(defn ^:publish & ;; a.k.a. comma or span
  ([] (mfn mfn-span0 {:mf-expr '(&)} [x] ())) ;; a.k.a. empty
  ([mf1] mf1)
  ([mf1 & mfs]
   (mfn mfn-spann {:mfc-expr `(& ~mf1 ~@mfs)} [x]
        (mapcat #(invoke % x) (cons mf1 mfs)))))

(defn ^:publish | ;; pipe: variatic monoid-plus over monadic fns
  ([] list) ;; a.k.a. unit
  ([mf1] mf1)
  ([mf1 & mfs]
   (mfn mfn-pipen {:mfc-expr `(| ~mf1 ~@mfs)} [x]
        (reduce (fn [mx mf] (mapcat #(invoke mf %) mx))
                (list x)
                (cons mf1 mfs)))))

(def ^:publish .
  (mfn mfn-dot {:mf-expr '.} [x]
       (list x)))

;; The list monad is additive, so it also supplies an mzero and mplus
;; Its mplus would be apply concat

(defn mk-each [getter]
  (fn [mf]
    (mfn mfn-all {:mf-expr `(each ~getter)} [x]
         (mapcat (fn [coll-nav]
                   (let [coll (navigate coll-nav)]
                     (ex-assert (coll? coll)
                                (str "`each` requires a seqable collection, not "
                                     (type coll) ": " (pr-str coll)))
                     (map-indexed (fn [i _] (getter coll-nav i)) coll)))
                 (invoke mf x)))))

(def ^:publish each
  (mk-each nav-get))

(def-mfc path [mf] [x]
  (map chart (invoke mf (navigate x))))

(def-mfc if [b t e] [x]
  (mapcat #(invoke (if % t e) x) (invoke-value b x)))

(defmacro ^{:publish 'let} cq-let
  [[sym sym-mf] mf]
  `(mfn ~'mfn-cq-let {:mfc-expr '~(list 'cq-let [sym sym-mf] mf)} [x#]
        (->>
         (invoke ~sym-mf x#)
         (mapcat (fn [value#]
                   (let [~sym (constantly (list value#))]
                     (invoke ~mf x#)))))))

(defmacro ^{:publish 'letfn} cq-letfn
  [fnforms mf]
  `(mfn ~'mfn-cq-letfn {:mfc-expr '~(list 'cq-letfn fnforms mf)} [x#]
        (letfn ~fnforms (invoke ~mf x#))))

(defmacro ^{:publish 'letlift} cq-letlift
  [fn-vec mf]
  `(mfn ~'mfn-cq-letlift {:mfc-expr '~(list 'cq-letlift fn-vec mf)} [x#]
        (let [~@(mapcat (fn [fn-sym]
                          [fn-sym `(lift ~fn-sym
                                         {:sym '~(symbol "lifted"
                                                         (name fn-sym))})])
                        fn-vec)]
          (invoke ~mf x#))))

(def-mfc select [value-mf test-mf] [x]
  (let [value-nav-stream (invoke value-mf x)]
    (mapcat #(when % value-nav-stream)
            (invoke-value test-mf x))))

;; EXPERIMENTAL dynamically rooted paths
(comment
  (def ^:publish rooted
    (mfn mfn-rooted {:mf-expr 'rooted} [x]
         (list (navigate x))))

  (def ^:publish rooted-path
    (mfn mfn-rooted-path {:mf-expr 'rooted-path} [x]
         (list (chart x))))

  (def-mfc rooted-reset [mf] [x]
    (list
     (let [deep-value (navigate (first (invoke mf x)))]
       (assoc-in (get-root x) (chart x) deep-value)))))

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

;; lifted functions

(defmacro def-lift [sym clj-fn & [local-alias]]
  `(def ~(with-meta (or local-alias (symbol (str "cq-" sym)))
           {:publish (str sym)})
     (lift ~clj-fn {:sym '~sym})))

(def-lift concat clj/concat)
(def-lift first clj/first)
(def-lift inc clj/inc)
(def-lift + clj/+)
(def-lift - clj/-)
(def-lift * clj/*)
(def-lift / clj// cq-divide)
(def-lift = clj/=)
(def-lift not= clj/not=)
(def-lift > clj/>)
(def-lift < clj/<)
(def-lift >= clj/>=)
(def-lift <= clj/<=)
(def-lift vector clj/vector)

;; These are not like the jq functions; they return all combinatorics because we
;; think that makes sense. They do not short-circuit:
(def-lift and (fn [a b] (and a b)))
(def-lift or  (fn [a b] (or  a b)))

(def-lift str #(apply str %&))

