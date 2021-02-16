(ns net.n01se.cq.core)

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

(defmethod print-method ValuePath [o w]
  (.write w (str "#cq/ValuePath" {:root (.root-value o)
                                  :path (.path o)})))

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

(declare &)
(declare invoke-value)

(defn invoke [mf x]
  (cond
    (number? mf) (list mf)
    (string? mf) (list mf)
    (boolean? mf) (list mf)
    (vector? mf) (list (vec (invoke-value (apply & mf) x)))
    (fn? mf) (mf x)
    (var? mf) (mf x)
    (nil? mf) (list nil) ;; Complain? jq doesn't
    :else (throw (ex-info (str "Can't invoke " (pr-str mf)) {}))))

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
  `(defn ~sym [& args#]
     (let [~arg-syms args#]
       (mfn ~(symbol (str "mfn-" sym)) {:mfc-expr (cons '~sym args#)} [~this-sym]
            ~@body))))

;; There is a list monad at the base of our operations:
;; ...where "unit" is list and "bind" is mapcat except the arguments are reversed

;; monoid-plus over monadic vals obtained by invoking each mf with x
(def-mfc & [& mfs] [x] ;; a.k.a. comma or span
  (mapcat #(invoke % x) mfs))
