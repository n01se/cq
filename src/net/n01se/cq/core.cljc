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

(def dot
  (ffn ffn-dot [x]
       (list (as-value-path x))))
