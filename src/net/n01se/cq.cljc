(ns net.n01se.cq
  (:require [cheshire.core :as json]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]))

;; PathExpr
(defrecord PathExpr [path value])

(defn as-path-expr [x]
  (if (instance? PathExpr x)
    x
    (PathExpr. [] x)))

(defn get-value [x]
  (if (instance? PathExpr x)
    (:value x)
    x))

(defn get-path [x]
  (if (instance? PathExpr x)
    (:path x)
    (throw (ex-info (str "Invalid path expression: " x) {:path x}))))

;; Testing
(defonce jq-cache (memoize (fn [jqs] (sh "jq" "-nc" jqs))))

(defn check-jq [cq jqs]
  (if-not (string? jqs)
    true
    (let [jq (map json/parse-string (str/split (:out (jq-cache jqs)) #"\n"))]
      (when-not (= cq jq)
        (prn :expected jq :got cq))
      (= cq jq))))

(defmacro deft [n jq pipe]
  `(def ~(vary-meta n assoc
                    :test `(fn []
                             (let [cq# (invoke-value ~pipe nil)]
                               (assert (check-jq cq# ~jq) (str))
                               cq#)))
     (fn [] ((:test (meta (var ~n)))))))

(defn test-here []
  (dorun (map #(%) (keep (comp :test meta val) (ns-publics *ns*))))
  :ok)

;; from https://github.com/clojure/math.combinatorics/blob/61f2a03cc941d68f2d9889b0c0f224e3067d1088/src/main/clojure/clojure/math/combinatorics.cljc#L232-L249
(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(def hole (reify Object (toString [_] "hole")))

(declare comma)

(defn invoke [f x]
  (cond
    (= f hole)  (list hole)
    (number? f) (list f)
    (string? f) (list f)
    (vector? f) (list (vec ((apply comma f) x)))
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
    (mapcat #(invoke-value % x) exprs)))

(defn dot [x]
  (list (as-path-expr x)))

(defn all [x]
  (let [{:keys [path value]} (as-path-expr x)]
    (map-indexed (fn [i y] (PathExpr. (conj path i) y)) value)))

(defn path [f]
  (fn [x]
    (map get-path (invoke f x))))

(defn my-first [f]
  (fn [x]
    (take 1 (invoke-value f x))))

(defn lift [f & [{:keys [apply-dot?]}]]
  (fn lifted [& fs]
    (fn eval-lift [x]
      (map #(apply f (cond-> (reverse %)
                       apply-dot? (conj (get-value x))))
           (apply cartesian-product (reverse (map #(invoke-value % x) fs)))))))

(def my-get (lift get {:apply-dot? true}))
(def times (lift *))
(def plus (lift +))
(def minus (lift -))

;; .[] all
;; .   dot

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
        (my-get (comma 1 2))))

(deft t16 "[1,2,3],[4,5,6],[7,8,9] | .[.[]]"
  (pipe (comma [1 2 3] [4 5 6] [7 8 9])
        (my-get all)))

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
        (my-first [dot])))

(deft t11 "1,2,3 | [4,.]"
  (pipe (comma 1 2 3)
        (comma [4 dot])))

(deft t10 "[1,2,3] | first(.[] | . + 1)"
  (pipe [1 2 3]
        (my-first (pipe
                   all
                   (plus dot 1)))))

(deft t9 "[1,2,3] | first(.[])"
  (pipe [1 2 3]
        (my-first all)))

(deft t8 "[[1],[2],[3]] | .[] | .[0]"
  (pipe (comma [[1] [2] [3]])
        all
        (my-get 0)))

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
        (my-get 0)))

(deft t0 "1,2,3 | . + 1"
  (pipe (comma 1 2 3)
        (plus dot 1)))
