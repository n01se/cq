(ns net.n01se.test-cq
  (:refer-clojure :exclude [concat inc + - * / = not= < > <= >=])
  (:require [net.n01se.cq
             :as cq
             :refer [. | & all hole path lift modify
                     rooted rooted-path rooted-reset
                     select cq-get cq-letlift cq-letfn
                     cq-let cq-first
                     concat inc + - * / = not= < > <= >=]]
            [clojure.core :as clj]
            [net.n01se.cq.jq-compiler :as jqc]
            [clojure.java.shell :refer [sh]]
            [cheshire.core :as json]
            [clojure.string :as str]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

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
      (when-not (clj/= cq jq-out)
        (prn :expected jq-out :got cq))
      (clj/= cq jq-out))))

(defmacro deft [n jq expr]
  `(def ~(vary-meta n assoc
                    :jq jq
                    :test `(fn []
                             (let [cq# (cq/cq ~expr)]
                               (assert (check-jq cq# ~jq))
                               cq#)))
     (fn [] ((:test (meta (var ~n)))))))

(defn test-here []
  (dorun (map #(%) (keep (comp :test meta val) (ns-publics *ns*))))
  :ok)

(defn test-compiler []
  (doseq [[_ v] (ns-publics *ns*)]
    (when-let [jq (-> v meta :jq)]
      (let [form (-> jq jqc/parse jqc/jq-compile)]
        (assert (check-jq (cq/cq (eval form)) jq)))))
  :ok)

;; EXPERIMENTAL dynamically rooted paths

(deft tx3
  "[0],[3,4],[1],[2,5]" ;; is this right?
  #_"def f(p): [p] | path(.[]),(.[] |= .+1); [2,4] | f(.[0,1])"  ;; or this? [0] [1] [3 5]
  (| [2 4]
     rooted
     (cq-get (& 0 1))
     (& rooted-path
        (rooted-reset (+ . 1)))))

(deft tx2 "def f(p): path(p),p |= .+1; [[1]],[[5]] | .[0] | f(.[0])"
  (| (& [[1]] [[5]])
     (cq-get 0)
     rooted
     (cq-get 0)
     (& rooted-path
       (rooted-reset (+ . 1)))))

(deft tx1 "def f(p): path(p),p |= .+1; [[1]],[[5]] | f(.[0] | .[0])"
  (| (& [[1]] [[5]])
     rooted
     (cq-get 0)
     (cq-get 0)
     (& rooted-path
       (rooted-reset (+ . 1)))))

(deft tx0 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (| [[5]]
     rooted
     (cq-get 0)
     (cq-get 0)
     (& rooted-path
       (rooted-reset (+ . 1)))))

;; .[] all
;; .   .
;; =   put
;; |=  modify

(deft t38 "5 | [., 10 + .]"
  (cq-letlift [list]
    (| 5 (list . (+ 10 .)))))

(deft t37 "[3,-2,5,1,-3] | .[] | select(. > 0) |= .*2"
  #_(& 6 -2 10 2 -3)
  (| [3 -2 5 1 -3]
     all
     (modify (select (> . 0))
             (* . 2))))

(deft t36  "1,2,3 | select((.*2,.) != 2)"
  (| (& 1 2 3)
     (select (not= (& (* . 2) .) 2))))

(deft t35  "1,2,3 | select(. != 2)"
  (| (& 1 2 3)
     (select (not= . 2))))

(deft t34 "1,2,3,4,3,2,1 | (if . == 2 then empty else . end)"
  (| (& 1 2 3 4 3 2 1)
     (fn [x] (if (clj/= x 2) () (list x)))))

(deft t33 "def addvalue(f): f as $x | .[] | [. + $x]; [[1,2],[10,20]] | addvalue(.[0])"
  (cq-letfn [(addvalue [f] (cq-let [$x f]
                                   (| all [(concat . $x)])))]
            (| [[1,2],[10,20]]
               (addvalue (cq-get 0)))))

(deft t32 "def foo(f): f|f; 5|foo(.*2)"
  (let [foo (fn [f] (| f f))]
    (| 5 (foo (* . 2)))))

(deft t31 "[3,4] | [.[],9] as $a | .[],$a[]"
  (| [3 4]
     (cq-let [$a [all 9]]
             (& all (| $a all)))))

(deft t30  "(1,2,3) as $a | $a + 1"
  (cq-let [$a (& 1 2 3)]
          (+ $a 1)))

(deft t29 "5 as $a | $a"
  (cq-let [$a 5] $a))

(deft t28 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (let [f (fn [p]
            (& (path p)
               (modify p (+ . 1))))]
    (| [[5]]
       (f (| (cq-get 0) (cq-get 0))))))

(deft t27 "[[7],[8],[9]] | .[] |= ( .[] |= .+1 )"
  (| [[7] [8] [9]]
     (modify all
             (modify all
                     (+ . 1)))))

(deft t26 "[[7],[8],[9]] | (.[] | .[0]) |= .+1"
  (| [[7] [8] [9]]
     (modify (| all (cq-get 0))
             (+ . 1))))

(deft t25 "[[7],[8],[9]] | .[] | .[0] |= .+1"
  (| [[7] [8] [9]]
     all
     (modify (cq-get 0) (+ . 1))))

(deft t24 "[1,2,3] | (.[0,1]) |= .*2"
  (| [1 2 3]
     (modify (cq-get (& 0 1))
             (* . 2))))

(deft t23 "[4,5,6] | .[0,1]"
  (| [4 5 6] (cq-get (& 0 1))))

(deft t22 "path(.[0] | .[1] | .[2])"
  (path (| (cq-get 0)
           (cq-get 1)
           (cq-get 2))))

(deft t21 "[0,[[1]]] | .[1][0][0] |= . + 5"
  (| [0 [[1]]]
    (modify (| (cq-get 1)
               (cq-get 0)
               (cq-get 0))
            (+ . 5))))

(deft t20 "5 | (1*.,2) - (10*.,20) - (100*.,200)"
  (| 5
     (- (& (* 1 .) 2)
        (& (* 10 .) 20)
        (& (* 100 .) 200))))

(deft t19 "(1,2) - (10,20) - (100,200)"
  (- (& 1 2)
     (& 10 20)
     (& 100 200)))

(deft t18 "(1,2) + (10,20) + (100,200)"
  (+ (& 1 2)
     (& 10 20)
     (& 100 200)))

(deft t17 "[1,2,3],[4,5,6],[7,8,9] | .[1,2]"
  (| (& [1 2 3] [4 5 6] [7 8 9])
     (cq-get (& 1 2))))

(deft t16 "[1,2,3],[4,5,6],[7,8,9] | .[.[]]"
  (| (& [1 2 3] [4 5 6] [7 8 9])
     (cq-get all)))

(deft t15 "[1,2,3] | path(.[])"
  (| [1 2 3]
     (path all)))

(deft t14 "[1,2,3] | . | .[]"
  (| [1 2 3] . all))

(deft t13 "1 | . | ."
  (| 1 . .))

(deft t12 "1 | first([.])"
  (| 1
     (cq-first [.])))

(deft t11 "1,2,3 | [4,.]"
  (| (& 1 2 3)
     (& [4 .])))

(deft t10 "[1,2,3] | first(.[] | . + 1)"
  (| [1 2 3]
     (cq-first (| all
                  (inc .)))))

(deft t9 "[1,2,3] | first(.[])"
  (| [1 2 3]
     (cq-first all)))

(deft t8 "[[1],[2],[3]] | .[] | .[0]"
  (| (& [[1] [2] [3]])
     all
     (cq-get 0)))

(deft t7 "[1,2,3] | .[]"
  (| (& [1 2 3]) all))

(deft t6 "1,2,3 | 99,empty,.+1"
  (| (& 1 2 3)
     (& 99 hole (+ . 1))))

(deft t5 "1,2,3 | 99,.+1"
  (| (& 1 2 3)
     (& 99 (+ . 1))))

(deft t4 "1,2,3 | 99,."
  (| (& 1 2 3)
     (& 99 .)))

(deft t3  "1,2,3 | 4,5"
  (| (& 1 2 3)
     (& 4 5)))

(deft t2 "1,empty,3 | . + 1"
  (| (& 1 hole 3)
     (+ . 1)))

(deft t1 "[1,2,3] | first"
  (| [1 2 3]
     (cq-get 0)))

(deft t0 "1,2,3 | . + 1"
  (| (& 1 2 3)
     (+ . 1)))
