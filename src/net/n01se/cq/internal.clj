(ns net.n01se.cq.internal
  (:require [cheshire.core :as json]
            [clojure.core :as clj]
            [clojure.string :as str]
            [clojure.walk :refer [postwalk]]))

(binding [*ns* *ns*] (in-ns 'net.n01se.cq))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

;; Navigation
(defprotocol INavigation
  (get-value [_])
  (get-path [_])
  ;; TODO: add replace (for =, vs modify's |=)
  (modify* [_ base f]))

(def DefaultINavigation
  {:get-value identity
   :get-path (constantly [])
   :modify* (fn [me base f] (f base))})

(extend nil     INavigation DefaultINavigation)
(extend Object  INavigation DefaultINavigation)

;; TODO move invoke/eval to above INavigation?
(declare &)
(declare invoke)
(declare invoke-value)
(declare cq-eval1)

(defn nav-get [parent idx]
  (reify
    Object (toString [_] (str "nav-get " (pr-str idx) " on " parent))
    INavigation
    (get-value [_] (get (get-value parent) idx))
    (get-path [_] (conj (get-path parent) idx))
    (modify* [me base f]
      (modify* parent base #(update % idx f)))))

(defn invoke [mf x]
  (cond
    (number? mf) (list mf)
    (string? mf) (list mf)
    (boolean? mf) (list mf)
    (keyword? mf) (list mf)
    (vector? mf) (list (vec (invoke-value (apply & mf) x)))
    (map? mf) (->> (apply concat mf)
                   (map #(invoke-value % x))
                   cartesian-product
                   (map #(apply array-map %)))
    (fn? mf) (mf x)
    (var? mf) (mf x)
    (nil? mf) (list nil) ;; Complain? jq doesn't
    :else (throw (ex-info (str "Can't invoke " (pr-str mf)) {}))))

;; TODO: get rid of this in favor of cq-eval everywhere
(defn invoke-value [mf x]
  (map get-value (invoke mf x)))

(declare emit)

(defn emit-meta [{:keys [mfc-expr mf-expr]}]
  (if-let [[f & args] mfc-expr]
    (cons f (map emit args))
    mf-expr))

(defn emit [mf]
  (cond
    (vector? mf) (mapv emit mf)
    :else (or (emit-meta (meta mf)) mf)))

(def ^:dynamic *tracing* false)
(def ^:dynamic *mfn-depth* 1)
(defn trace [& args]
  (when *tracing*
    (apply println (apply str (repeat *mfn-depth* " ")) args)))

;; There is a list monad at the base of our operations: ...where "unit" is list
;; (aka `.`) and "bind" is mapcat except the arguments are reversed

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
           (trace "Pipe" ~(first argv) "into" emitted#)
           (let [result# (binding [*mfn-depth* (inc *mfn-depth*)] (do ~@body))]
             (trace "Return" result# "from" emitted#)
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

(def-mfc get [mf] [x]
  (let [path-elems (invoke-value mf x)]
    (map #(nav-get x %) path-elems)))

(def-mfc modify [path-mf value-mf] [x]
  (let [xval (get-value x)]
    (list
     (reduce
      (fn [acc nav]
        ;; some versions before jq-1.6 use `last` instead of eval1's `first`:
        (modify* nav acc #(cq-eval1 % value-mf)))
      xval
      (invoke path-mf xval)))))

(defn ^{:publish 'eval} cq-eval
  ([mf] (invoke-value mf nil))
  ([input mf] (invoke-value mf input)))

(defn ^{:publish 'eval1} cq-eval1 ;; TODO: throw if more than one in return list?
  ([mf] (first (invoke-value mf nil)))
  ([input mf] (first (invoke-value mf input))))

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

(defmacro ex-assert [expr & [msg]]
  `(when-not ~expr
     (throw (ex-info ~(or msg (pr-str expr)) {:expr '~expr}))))

(def ^:publish all ;; TODO: rename to `each`?
  (mfn mfn-all {:mf-expr 'all} [x]
       (let [coll (get-value x)]
         (ex-assert (coll? coll)
                    (str "`all` requires a seqable collection, not "
                         (type coll) ": " (pr-str coll)))
         (map-indexed (fn [i _] (nav-get x i)) coll))))

(def-mfc path [mf] [x]
  (map get-path (invoke mf (get-value x))))

(def-mfc first [mf] [x]
  (take 1 (invoke-value mf x)))

(def-mfc if [b t e] [x]
  (mapcat #(invoke (if % t e) x) (invoke-value b x)))

(defmacro ^{:publish 'let} cq-let
  [[sym sym-mf] mf]
  `(mfn ~'mfn-cq-let {:mfc-expr '~(list 'cq-let [sym sym-mf] mf)} [x#]
        (let [value# (invoke ~sym-mf x#)
              ~sym (constantly value#)
              result# (invoke ~mf x#)]
          result#)))

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

(defmacro ^:publish with-tracing [& body]
  `(binding [internal/*tracing* 1]
     ~@body))


(def-mfc select [mf] [x]
  (mapcat #(when % (list x))
          (invoke-value mf x)))

;; EXPERIMENTAL dynamically rooted paths
(comment
  (def ^:publish rooted
    (mfn mfn-rooted {:mf-expr 'rooted} [x]
         (list (get-value x))))

  (def ^:publish rooted-path
    (mfn mfn-rooted-path {:mf-expr 'rooted-path} [x]
         (list (get-path x))))

  (def-mfc rooted-reset [mf] [x]
    (list
     (let [deep-value (get-value (first (invoke mf x)))]
       (assoc-in (get-root x) (get-path x) deep-value)))))

(defn ^:publish lift [f & [{:keys [sym]}]]
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

;; lifted functions

(defmacro def-lift [sym clj-fn & [local-alias]]
  `(def ~(with-meta (or local-alias (symbol (str "cq-" sym)))
           {:publish (str sym)})
     (lift ~clj-fn {:sym '~sym})))

(def-lift concat clj/concat)
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

;; These are not like the jq functions; they return all combinatorics because we
;; think that makes sense. They do not short-circuit:
(def-lift and (fn [a b] (and a b)))
(def-lift or  (fn [a b] (or  a b)))


(def-lift jq-+ #(cond
                  (every? number? %&) (apply clj/+ %&)
                  (every? string? %&) (apply str %&)
                  :else (apply clj/concat %&)))

(def-lift str #(apply str %&))

(defn csv-str [x]
  (if (string? x)
    (str \" (str/replace x "\"" "\"\"") \")
    (str x)))
(defn tsv-str [x]
  (if (string? x)
    (str/replace x "\t" "\\t")
    (str x)))

(def-mfc jq-format [ftype] [x]
  (list
   (case ftype
     :text x
     :json (json/generate-string x)
     :csv (str/join "," (map csv-str x))
     :tsv (str/join "\t" (map tsv-str x))
     :html (apply str (map #(case %
                              \' "&apos;"
                              \" "&quot;"
                              \& "&amp;"
                              \< "&lt;"
                              \> "&gt;"
                              %) x))
     :uri (str/replace x #"[^\w']" (fn [s]
                                     (->> (.getBytes s)
                                          (map #(format "%%%02X" %))
                                          (apply str))))
     :sh (str \' (str/replace x #"'" "'\\\\''") \')
     :base64 (String. (.encode (java.util.Base64/getEncoder) (.getBytes x)) "UTF-8")
     :base64d (String. (.decode (java.util.Base64/getDecoder) x))
     x)))

(def ^:publish tojson
  (mfn mfn-tojson {:mf-expr 'tojson} [x]
       (list (json/generate-string (get-value x)))))

(def ^:publish fromjson
  (mfn mfn-fromjson {:mf-expr 'fromjson} [x]
       (list (json/parse-string (get-value x)))))

(def-mfc try [expr] [x]
  (try (doall (invoke-value expr x))
       (catch Exception ex ())))

(def ^:publish jq-tree-seq
  (mfn mfn-jq-tree-seq {:mf-expr 'jq-tree-seq} [x]
       (tree-seq coll?
                 #(if (map? %) (vals %) (seq %))
                 x)))


