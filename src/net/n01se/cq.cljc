(ns net.n01se.cq
  (:require [cheshire.core :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

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

;; Testing
(defonce jq-cache (memoize (fn [jqs] (sh "jq" "-nc" jqs))))

(defn parse-jq-out [string]
  (map json/parse-string (str/split string #"\n")))

(defn jq [string]
  (let [{:keys [out err exit]} (jq-cache string)]
    (when (not (zero? exit))
      (throw (Exception. err)))
    (parse-jq-out out)))

(defn check-jq [cq jqs]
  (if-not (string? jqs)
    true
    (let [jq-out (jq jqs)]
      (when-not (= cq jq-out)
        (prn :expected jq-out :got cq))
      (= cq jq-out))))

(defmacro deft [n jq expr]
  `(def ~(vary-meta n assoc
                    :test `(fn []
                             (let [cq# (invoke-value ~expr nil)]
                               (assert (check-jq cq# ~jq) (str))
                               cq#)))
     (fn [] ((:test (meta (var ~n)))))))

(defn test-here []
  (dorun (map #(%) (keep (comp :test meta val) (ns-publics *ns*))))
  :ok)

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
    (nil? mf) nil ;; Complain? jq doesn't
    :else (throw (ex-info (str "Can't invoke " (pr-str mf)) {}))))

(defn invoke-value [mf x]
  (map get-value (invoke mf x)))

(defn cq [mf & [x]]
  (invoke-value mf x))

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

(defn lift [f]
  (fn lifted [& mf]
    (ffn ffn-lift [x]
      (map #(apply f %)
           (cartesian-product (map #(invoke-value % x) mf))))))

(def times (lift *))
(def plus (lift +))
(def minus (lift -))

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

(deft tx3
  "[0],[3,4],[1],[2,5]" ;; is this right?
  #_"def f(p): [p] | path(.[]),(.[] |= .+1); [2,4] | f(.[0,1])"  ;; or this? [0] [1] [3 5]
  (pipe [2 4]
        rooted
        (cq-get (span 0 1))
        (span rooted-path
              (rooted-reset (plus dot 1)))))

(deft tx2 "def f(p): path(p),p |= .+1; [[1]],[[5]] | .[0] | f(.[0])"
  (pipe (span [[1]] [[5]])
        (cq-get 0)
        rooted
        (cq-get 0)
        (span
         rooted-path
         (rooted-reset (plus dot 1)))))

(deft tx1 "def f(p): path(p),p |= .+1; [[1]],[[5]] | f(.[0] | .[0])"
  (pipe (span [[1]] [[5]])
        rooted
        (cq-get 0)
        (cq-get 0)
        (span
         rooted-path
         (rooted-reset (plus dot 1)))))

(deft tx0 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (pipe [[5]]
        rooted
        (cq-get 0)
        (cq-get 0)
        (span
         rooted-path
         (rooted-reset (plus dot 1)))))

;; .[] all
;; .   dot
;; =   put
;; |=  modify

(deft t37 "[3,-2,5,1,-3] | .[] | select(. > 0) |= .*2"
  #_(span 6 -2 10 2 -3)
  (pipe [3 -2 5 1 -3]
        all
        (modify (select ((lift >) dot 0))
                ((lift *) dot 2))))

(deft t36  "1,2,3 | select((.*2,.) != 2)"
  (pipe (span 1 2 3)
        (select ((lift not=) (span (times dot 2) dot) 2))))

(deft t35  "1,2,3 | select(. != 2)"
  (pipe (span 1 2 3)
        (select ((lift not=) dot 2))))

(deft t34 "1,2,3,4,3,2,1 | (if . == 2 then empty else . end)"
  (pipe (span 1 2 3 4 3 2 1)
        (fn [x] (if (= x 2) () (list x)))))

(deft t33 "def addvalue(f): f as $x | .[] | [. + $x]; [[1,2],[10,20]] | addvalue(.[0])"
  (cq-letfn [(addvalue [f] (cq-let [$x f]
                                   (pipe all [((lift concat) dot $x)])))]
            (pipe [[1,2],[10,20]]
                  (addvalue (cq-get 0)))))

(deft t32 "def foo(f): f|f; 5|foo(.*2)"
  (let [foo (fn [f] (pipe f f))]
    (pipe 5 (foo (times dot 2)))))

(deft t31 "[3,4] | [.[],9] as $a | .[],$a[]"
  (pipe [3 4]
        (cq-let [$a [all 9]]
                (span all (pipe $a all)))))

(deft t30  "(1,2,3) as $a | $a + 1"
  (cq-let [$a (span 1 2 3)]
          (plus $a 1)))

(deft t29 "5 as $a | $a"
  (cq-let [$a 5] $a))

(deft t28 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (let [f (fn [p] (span (path p)
                        (modify p (plus dot 1))))]
    (pipe [[5]]
          (f (pipe (cq-get 0) (cq-get 0))))))

(deft t27 "[[7],[8],[9]] | .[] |= ( .[] |= .+1 )"
  (pipe [[7] [8] [9]]
        (modify all
                  (modify all
                            (plus dot 1)))))

(deft t26 "[[7],[8],[9]] | (.[] | .[0]) |= .+1"
  (pipe [[7] [8] [9]]
        (modify (pipe all (cq-get 0))
                  (plus dot 1))))

(deft t25 "[[7],[8],[9]] | .[] | .[0] |= .+1"
  (pipe [[7] [8] [9]]
        all
        (modify (cq-get 0) (plus dot 1))))

(deft t24 "[1,2,3] | (.[0,1]) |= .*2"
  (pipe [1 2 3]
        (modify (cq-get (span 0 1))
                (times dot 2))))

(deft t23 "[4,5,6] | .[0,1]"
  (pipe [4 5 6] (cq-get (span 0 1))))

(deft t22 "path(.[0] | .[1] | .[2])"
  (path (pipe (cq-get 0)
              (cq-get 1)
              (cq-get 2))))

(deft t21 "[0,[[1]]] | .[1][0][0] |= . + 5"
  (pipe [0 [[1]]]
        (modify
         (pipe (cq-get 1) (cq-get 0) (cq-get 0))
         (plus dot 5))))

(deft t20 "5 | (1*.,2) - (10*.,20) - (100*.,200)"
  (pipe 5
        (minus (span (times 1 dot) 2)
               (span (times 10 dot) 20)
               (span (times 100 dot) 200))))

(deft t19 "(1,2) - (10,20) - (100,200)"
  (minus (span 1 2)
         (span 10 20)
         (span 100 200)))

(deft t18 "(1,2) + (10,20) + (100,200)"
  (plus (span 1 2)
        (span 10 20)
        (span 100 200)))

(deft t17 "[1,2,3],[4,5,6],[7,8,9] | .[1,2]"
  (pipe (span [1 2 3] [4 5 6] [7 8 9])
        (cq-get (span 1 2))))

(deft t16 "[1,2,3],[4,5,6],[7,8,9] | .[.[]]"
  (pipe (span [1 2 3] [4 5 6] [7 8 9])
        (cq-get all)))

(deft t15 "[1,2,3] | path(.[])"
  (pipe [1 2 3]
        (path all)))

(deft t14 "[1,2,3] | . | .[]"
  (pipe [1 2 3]
        dot
        all))

(deft t13 "1 | . | ."
  (pipe 1
        dot
        dot))

(deft t12 "1 | first([.])"
  (pipe 1
        (cq-first [dot])))

(deft t11 "1,2,3 | [4,.]"
  (pipe (span 1 2 3)
        (span [4 dot])))

(deft t10 "[1,2,3] | first(.[] | . + 1)"
  (pipe [1 2 3]
        (cq-first (pipe
                   all
                   (plus dot 1)))))

(deft t9 "[1,2,3] | first(.[])"
  (pipe [1 2 3]
        (cq-first all)))

(deft t8 "[[1],[2],[3]] | .[] | .[0]"
  (pipe (span [[1] [2] [3]])
        all
        (cq-get 0)))

(deft t7 "[1,2,3] | .[]"
  (pipe (span [1 2 3]) all))

(deft t6 "1,2,3 | 99,empty,.+1"
  (pipe (span 1 2 3)
        (span 99 hole (plus dot 1))))

(deft t5 "1,2,3 | 99,.+1"
  (pipe (span 1 2 3)
        (span 99 (plus dot 1))))

(deft t4 "1,2,3 | 99,."
  (pipe (span 1 2 3)
        (span 99 dot)))

(deft t3  "1,2,3 | 4,5"
  (pipe (span 1 2 3)
        (span 4 5)))

(deft t2 "1,empty,3 | . + 1"
  (pipe (span 1 hole 3)
        (plus dot 1)))

(deft t1 "[1,2,3] | first"
  (pipe [1 2 3]
        (cq-get 0)))

(deft t0 "1,2,3 | . + 1"
  (pipe (span 1 2 3)
        (plus dot 1)))
