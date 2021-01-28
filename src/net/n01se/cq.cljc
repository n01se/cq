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

(defn invoke [f x]
  (cond
    (= f hole)  (list hole)
    (number? f) (list f)
    (string? f) (list f)
    (vector? f) (list (vec (invoke-value (apply comma f) x)))
    (fn? f) (f x)
    :else (throw (ex-info (str "Can't invoke " (pr-str f)) {}))))

(defn invoke-value [f x]
  (map get-value (invoke f x)))

(defn pipe [& fns]
  (fn [init]
    (reduce (fn [s f] (remove #(= hole %) (mapcat #(invoke f %) s)))
            (remove #(= hole %) (list init))
            fns)))

(defn comma [& exprs]
  (fn [x]
    (mapcat #(invoke % x) exprs)))

(defn dot [x]
  (list (as-value-path x)))

(defn all [x]
  (map-indexed (fn [i y] (update-path x conj i)) (get-value x)))

(defn path [f]
  (fn [x]
    (map get-path (invoke f (reroot-path x)))))

(defn cq-first [f]
  (fn [x]
    (take 1 (invoke-value f x))))

(defn cq-update [fa fb]
  (fn [x]
    (update-in x ((path fa) x) (fn [x] (invoke-value fb x)))))

(defn lift [f]
  (fn lifted [& fs]
    (fn eval-lift [x]
      (map #(apply f %)
           (cartesian-product (map #(invoke-value % x) fs))))))

(def times (lift *))
(def plus (lift +))
(def minus (lift -))

(defn cq-get [idx-fn]
  (fn [x]
    (let [path-elems (invoke-value idx-fn x)]
      (map #(update-path x conj %) path-elems))))

(defn cq-update-in [path-expr value-expr]
  (fn [x]
    (list
     (reduce
      (fn [x value-path]
        ;; some versions before jq-1.6 use `last` instead of `first`:
        (let [deep-value (get-value (first (invoke value-expr value-path)))]
          (assoc-in x (get-path value-path) deep-value)))
      (get-value x)
      (invoke path-expr (reroot-path x))))))


;; EXPERIMENTAL dynamically rooted paths
(defn rooted [x]
  (list (reroot-path x)))

(defn rooted-path [x]
  (list (get-path x)))

(defn rooted-update-in [value-expr]
  (fn [x]
    (list
     (let [deep-value (get-value (first (invoke value-expr x)))]
       (assoc-in (get-root x) (get-path x) deep-value)))))

(deft t31
  "[0],[3,4],[1],[2,5]" ;; is this right?
  #_"def f(p): [p] | path(.[]),(.[] |= .+1); [2,4] | f(.[0,1])"  ;; or this? [0] [1] [3 5]
  (pipe [2 4]
        rooted
        (cq-get (comma 0 1))
        (comma rooted-path
               (rooted-update-in (plus dot 1)))))

(deft t30 "def f(p): path(p),p |= .+1; [[1]],[[5]] | .[0] | f(.[0])"
  (pipe (comma [[1]] [[5]])
        (cq-get 0)
        rooted
        (cq-get 0)
        (comma
         rooted-path
         (rooted-update-in (plus dot 1)))))

(deft t29 "def f(p): path(p),p |= .+1; [[1]],[[5]] | f(.[0] | .[0])"
  (pipe (comma [[1]] [[5]])
        rooted
        (cq-get 0)
        (cq-get 0)
        (comma
         rooted-path
         (rooted-update-in (plus dot 1)))))

(deft t28 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (pipe [[5]]
        rooted
        (cq-get 0)
        (cq-get 0)
        (comma
         rooted-path
         (rooted-update-in (plus dot 1)))))

;; .[] all
;; .   dot
;; |=  cq-update-in

(deft t27 "[[7],[8],[9]] | .[] |= ( .[] |= .+1 )"
  (pipe [[7] [8] [9]]
        (cq-update-in all
                      (cq-update-in all
                                    (plus dot 1)))))

(deft t26 "[[7],[8],[9]] | (.[] | .[0]) |= .+1"
  (pipe [[7] [8] [9]]
        (cq-update-in (pipe all (cq-get 0))
                      (plus dot 1))))

(deft t25 "[[7],[8],[9]] | .[] | .[0] |= .+1"
  (pipe [[7] [8] [9]]
        all
        (cq-update-in (cq-get 0) (plus dot 1))))

(deft t24 "[1,2,3] | (.[0,1]) |= .*2"
  (pipe [1 2 3]
        (cq-update-in (cq-get (comma 0 1))
                      (times dot 2))))

(deft t23 "[4,5,6] | .[0,1]"
  (pipe [4 5 6] (cq-get (comma 0 1))))

(deft t22 "path(.[0] | .[1] | .[2])"
  (path (pipe (cq-get 0)
              (cq-get 1)
              (cq-get 2))))

(deft t21 "[0,[[1]]] | .[1][0][0] |= . + 5"
  (pipe [0 [[1]]]
        (cq-update-in
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
