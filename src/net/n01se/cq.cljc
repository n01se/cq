(ns net.n01se.cq
  (:require [cheshire.core :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

;; ValuePath
(defprotocol IValuePath
  (get-value [_] "Return the resolved, normal Clojure value")
  (get-root [_] "Return the root, normal Clojure value. Throw if not ValuePath")
  (get-path [_] "Return the path vector")
  (update-path* [x f args]
    "Return ValuePath with same root as x and path of (apply f old-path args).
     If x is not a ValuePath, return a ValuePath rooted at x with value x"))

(defrecord ValuePath [root-value path]
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

(def hole (reify Object (toString [_] "hole")))

(declare comma)
(declare invoke-value)

(defn invoke [f topic]
  (cond
    (= f hole)  (list hole)
    (number? f) (list f)
    (string? f) (list f)
    (vector? f) (list (vec (invoke-value (apply comma f) topic)))
    (fn? f) (f topic)
    (nil? f) nil ;; Complain? jq doesn't
    :else (throw (ex-info (str "Can't invoke " (pr-str f)) {}))))

(defn invoke-value [f topic]
  (map get-value (invoke f topic)))

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

(defn bind [s f]
  (remove #(= hole %) (mapcat #(invoke f %) s)))

(defn pipe [& fns]
  (ffn ffn-pipe [init]
    (reduce bind (remove #(= hole %) (list init)) fns)))

(defn comma [& exprs]
  (ffn ffn-comma [topic]
    (mapcat (ffn ffn-comma-part [y] (invoke y topic)) exprs)))

(def dot
  (ffn ffn-dot [topic]
       (list (as-value-path topic))))

(def all
  (ffn ffn-all [topic]
       (map-indexed (fn [i y] (update-path topic conj i)) (get-value topic))))

(defn path [f]
  (ffn ffn-path [topic]
    (map get-path (invoke f (reroot-path topic)))))

(defn cq-first [f]
  (ffn ffn-first [topic]
    (take 1 (invoke-value f topic))))

(defmacro cq-let [[sym sym-expr] expr]
  `(ffn ~'ffn-cq-let [topic#]
     (let [value# (invoke ~sym-expr topic#)
           ~sym (constantly value#)
           result# (invoke ~expr topic#)]
       result#)))

(defmacro cq-letfn [fnforms expr]
  `(ffn ~'ffn-cq-letfn [topic#]
        (letfn ~fnforms (invoke ~expr topic#))))

(defn lift [f]
  (fn lifted [& fs]
    (ffn ffn-lift [topic]
      (map #(apply f %)
           (cartesian-product (map #(invoke-value % topic) fs))))))

(def times (lift *))
(def plus (lift +))
(def minus (lift -))

(defn cq-get [idx-fn]
  (ffn ffn-get [topic]
    (let [path-elems (invoke-value idx-fn topic)]
      (map #(update-path topic conj %) path-elems))))

(defn cq-reset [path-expr value-expr]
  (ffn ffn-reset [topic]
    (list
     (reduce
      (fn [acc value-path]
        ;; some versions before jq-1.6 use `last` instead of `first`:
        (let [deep-value (get-value (first (invoke value-expr value-path)))
              path (get-path value-path)]
          (if (empty? path)
            deep-value ;; c'mon, Clojure!
            (assoc-in acc path deep-value))))
      (get-value topic)
      (invoke path-expr (reroot-path topic))))))

(defn select [f]
  (ffn ffn-select [topic]
       (mapcat #(when % (list topic))
               (invoke-value f topic))))


;; EXPERIMENTAL dynamically rooted paths
(def rooted
  (ffn ffn-rooted [topic]
       (list (reroot-path topic))))

(def rooted-path
  (ffn ffn-rooted-path [topic]
       (list (get-path topic))))

(defn rooted-reset [value-expr]
  (ffn ffn-rooted-reset [topic]
    (list
     (let [deep-value (get-value (first (invoke value-expr topic)))]
       (assoc-in (get-root topic) (get-path topic) deep-value)))))

(deft tx3
  "[0],[3,4],[1],[2,5]" ;; is this right?
  #_"def f(p): [p] | path(.[]),(.[] |= .+1); [2,4] | f(.[0,1])"  ;; or this? [0] [1] [3 5]
  (pipe [2 4]
        rooted
        (cq-get (comma 0 1))
        (comma rooted-path
               (rooted-reset (plus dot 1)))))

(deft tx2 "def f(p): path(p),p |= .+1; [[1]],[[5]] | .[0] | f(.[0])"
  (pipe (comma [[1]] [[5]])
        (cq-get 0)
        rooted
        (cq-get 0)
        (comma
         rooted-path
         (rooted-reset (plus dot 1)))))

(deft tx1 "def f(p): path(p),p |= .+1; [[1]],[[5]] | f(.[0] | .[0])"
  (pipe (comma [[1]] [[5]])
        rooted
        (cq-get 0)
        (cq-get 0)
        (comma
         rooted-path
         (rooted-reset (plus dot 1)))))

(deft tx0 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (pipe [[5]]
        rooted
        (cq-get 0)
        (cq-get 0)
        (comma
         rooted-path
         (rooted-reset (plus dot 1)))))

;; .[] all
;; .   dot
;; =  cq-set
;; |=  cq-reset

(deft t37 "[3,-2,5,1,-3] | .[] | select(. > 0) |= .*2"
  #_(comma 6 -2 10 2 -3)
  (pipe [3 -2 5 1 -3]
        all
        (cq-reset (select ((lift >) dot 0))
                  ((lift *) dot 2))))

(deft t36  "1,2,3 | select((.*2,.) != 2)"
  (pipe (comma 1 2 3)
        (select ((lift not=) (comma (times dot 2) dot) 2))))

(deft t35  "1,2,3 | select(. != 2)"
  (pipe (comma 1 2 3)
        (select ((lift not=) dot 2))))

(deft t34 "1,2,3,4,3,2,1 | (if . == 2 then empty else . end)"
  (pipe (comma 1 2 3 4 3 2 1)
        (fn [x] (list (if (= x 2) hole x)))))

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
                (comma all (pipe $a all)))))

(deft t30  "(1,2,3) as $a | $a + 1"
  (cq-let [$a (comma 1 2 3)]
          (plus $a 1)))

(deft t29 "5 as $a | $a"
  (cq-let [$a 5] $a))

(deft t28 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (let [f (fn [p] (comma (path p)
                         (cq-reset p (plus dot 1))))]
    (pipe [[5]]
          (f (pipe (cq-get 0) (cq-get 0))))))

(deft t27 "[[7],[8],[9]] | .[] |= ( .[] |= .+1 )"
  (pipe [[7] [8] [9]]
        (cq-reset all
                  (cq-reset all
                            (plus dot 1)))))

(deft t26 "[[7],[8],[9]] | (.[] | .[0]) |= .+1"
  (pipe [[7] [8] [9]]
        (cq-reset (pipe all (cq-get 0))
                  (plus dot 1))))

(deft t25 "[[7],[8],[9]] | .[] | .[0] |= .+1"
  (pipe [[7] [8] [9]]
        all
        (cq-reset (cq-get 0) (plus dot 1))))

(deft t24 "[1,2,3] | (.[0,1]) |= .*2"
  (pipe [1 2 3]
        (cq-reset (cq-get (comma 0 1))
                  (times dot 2))))

(deft t23 "[4,5,6] | .[0,1]"
  (pipe [4 5 6] (cq-get (comma 0 1))))

(deft t22 "path(.[0] | .[1] | .[2])"
  (path (pipe (cq-get 0)
              (cq-get 1)
              (cq-get 2))))

(deft t21 "[0,[[1]]] | .[1][0][0] |= . + 5"
  (pipe [0 [[1]]]
        (cq-reset
         (pipe (cq-get 1) (cq-get 0) (cq-get 0))
         (plus dot 5))))

(deft t20 "5 | (1*.,2) - (10*.,20) - (100*.,200)"
  (pipe 5
        (minus (comma (times 1 dot) 2)
               (comma (times 10 dot) 20)
               (comma (times 100 dot) 200))))

(deft t19 "(1,2) - (10,20) - (100,200)"
  (minus (comma 1 2)
         (comma 10 20)
         (comma 100 200)))

(deft t18 "(1,2) + (10,20) + (100,200)"
  (plus (comma 1 2)
        (comma 10 20)
        (comma 100 200)))

(deft t17 "[1,2,3],[4,5,6],[7,8,9] | .[1,2]"
  (pipe (comma [1 2 3] [4 5 6] [7 8 9])
        (cq-get (comma 1 2))))

(deft t16 "[1,2,3],[4,5,6],[7,8,9] | .[.[]]"
  (pipe (comma [1 2 3] [4 5 6] [7 8 9])
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
  (pipe (comma 1 2 3)
        (comma [4 dot])))

(deft t10 "[1,2,3] | first(.[] | . + 1)"
  (pipe [1 2 3]
        (cq-first (pipe
                   all
                   (plus dot 1)))))

(deft t9 "[1,2,3] | first(.[])"
  (pipe [1 2 3]
        (cq-first all)))

(deft t8 "[[1],[2],[3]] | .[] | .[0]"
  (pipe (comma [[1] [2] [3]])
        all
        (cq-get 0)))

(deft t7 "[1,2,3] | .[]"
  (pipe (comma [1 2 3]) all))

(deft t6 "1,2,3 | 99,empty,.+1"
  (pipe (comma 1 2 3)
        (comma 99 hole (plus dot 1))))

(deft t5 "1,2,3 | 99,.+1"
  (pipe (comma 1 2 3)
        (comma 99 (plus dot 1))))

(deft t4 "1,2,3 | 99,."
  (pipe (comma 1 2 3)
        (comma 99 dot)))

(deft t3  "1,2,3 | 4,5"
  (pipe (comma 1 2 3)
        (comma 4 5)))

(deft t2 "1,empty,3 | . + 1"
  (pipe (comma 1 hole 3)
        (plus dot 1)))

(deft t1 "[1,2,3] | first"
  (pipe [1 2 3]
        (cq-get 0)))

(deft t0 "1,2,3 | . + 1"
  (pipe (comma 1 2 3)
        (plus dot 1)))
