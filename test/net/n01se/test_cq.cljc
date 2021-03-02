(ns net.n01se.test-cq
  (:require [net.n01se.cq :as cq :refer [. &]]
            [clojure.core :as clj]
            [clojure.java.io :as io]
            [net.n01se.cq.jq-compiler :as jqc]
            [net.n01se.cq.jq-compat :as jq]
            [clojure.java.shell :refer [sh]]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :as t]))

(def jq-cache
  (memoize
   (fn [jqs input]
     (if input
       (sh "jq" "-c" jqs :in input)
       (sh "jq" "-nc" jqs)))))

(defn parse-jq-out [string]
  (map json/parse-string (str/split string #"\n")))

(defn jq [string & [input]]
  (let [{:keys [out err exit]} (jq-cache string input)]
    (when (not (zero? exit))
      (throw (Exception. err)))
    (parse-jq-out out)))

(defn check-jq [cq jqs & [input]]
  (if-not (string? jqs)
    true
    (let [jq-out (jq jqs input)]
      (when-not (clj/= cq jq-out)
        (prn :expected jq-out :got cq))
      (clj/= cq jq-out))))

(defmacro deft [n jq expr]
  `(def ~(vary-meta n assoc
                    :jq jq
                    :test `(fn []
                             (let [cq# (cq/eval (cq/with-refer-all [~'cq] ~expr))]
                               (assert (check-jq cq# ~jq))
                               cq#)))
     (fn [] ((:test (meta (var ~n)))))))

(defn test-here []
  (dorun (map #(%) (keep (comp :test meta val) (ns-publics *ns*))))
  :ok)

(defn test-compiler []
  (doseq [[_ v] (ns-publics *ns*)]
    (when-let [jq (-> v meta :jq)]
      (let [cq-mfn (jqc/compile-str jq)]
        (when-not (check-jq (cq/eval cq-mfn) jq)
          (println "check failed: " jq)))))
  :ok)

(defn jq-unit-tests []
  (let [tests (slurp (io/resource "net/n01se/cq/jq.test"))]
    (doseq [[_ jq input & outs]
            (re-seq #"\n(?:#.*\n|\n)+(.+)\n\ufeff?(.+)\n(.+)(?:\n(.+))*" tests)]
      (prn jq)
      (when-not (= "%%FAIL" jq)
        (let [cq-mfn (-> jq jqc/compile-str)]
          (assert (check-jq (cq/eval (json/parse-string input) cq-mfn)
                            jq input)))))))

;; EXPERIMENTAL dynamically rooted paths

(comment
  (deft tx3
    "[0],[3,4],[1],[2,5]" ;; is this right?
    #_"def f(p): [p] | path(.[]),(.[] |= .+1); [2,4] | f(.[0,1])"  ;; or this? [0] [1] [3 5]
    (| [2 4]
       rooted
       (get . (& 0 1))
       (& rooted-path
          (rooted-reset (+ . 1)))))

  (deft tx2 "def f(p): path(p),p |= .+1; [[1]],[[5]] | .[0] | f(.[0])"
    (| (& [[1]] [[5]])
       (get . 0)
       rooted
       (get . 0)
       (& rooted-path
          (rooted-reset (+ . 1)))))

  (deft tx1 "def f(p): path(p),p |= .+1; [[1]],[[5]] | f(.[0] | .[0])"
    (| (& [[1]] [[5]])
       rooted
       (get . 0)
       (get . 0)
       (& rooted-path
          (rooted-reset (+ . 1)))))

  (deft tx0 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
    (| [[5]]
       rooted
       (get . 0)
       (get . 0)
       (& rooted-path
          (rooted-reset (+ . 1))))))

;; .[] all
;; .   .
;; =   assign
;; |=  modify

(deft t63 "[4,[3,2,1,0]][1][1:3][]"
  (-> [4 [3 2 1 0]]
      (get 1)
      (slice 1 3)
      each))

(deft t62 "(1,2,3) | (10,20,30) as $b | [.,$b]"
  (| (& 1 2 3)
     (let [$b (& 10 20 30)]
       [. $b])))

;; jq slice does combinations backwards, so work around it
(deft t61 "[1,2,3,4,5,6,7,8,9] | ((3,4) as $upto | .[(1,5):$upto]) = ((-1,-2) | [.])"
  (| [1 2 3 4 5 6 7 8 9]
     (assign (slice . (& 1 5) (& 3 4))
             [(& -1 -2)])))

(deft t60 "[8,7,6,5,4] | .[1:4] |= [.[] | .+10]"
  (| [8 7 6 5 4]
     (modify (slice . 1 4)
             (collect-into [] (+ (each .) 10)))))

(deft t59 "[8,7,6,5,4] | .[1:4] |= []"
  (| [8 7 6 5 4] (modify (slice . 1 4) [])))

(deft t58 "[8,7,6,5,4][1:4]"
  (| [8 7 6 5 4] (slice . 1 4)))

(deft t57 "(., .) = (42, 43)"
  (assign (& . .) (& 42 43)))

(deft t56 ". = (42, 43)"
  (assign . (& 42 43)))

(deft t55 ". = 42"
  (assign . 42))

(deft t45 "{a:9} | {a, (\"c\",\"d\"):10} | .a,.d"
  (| {:a 9}
     {:a (get . :a), (& :c :d) 10}
     (get . (& :a :d))))

(deft t44 "{a:1, b:2, c:3, d:\"c\"} | {a,b,(.d):.a,e:(.b,\"x\")}"
  (| {"a" 1, "b" 2, "c" 3, "d" "c"}
     {"a" (get . "a")
      "b" (get . "b")
      (get . "d") (get . "a"),
      "e" (& (get . "b") "x")}))

(deft t43 "{a:99} | .a"
  (| {"a" 99} (get . "a")))

(deft t42 "42 | {a:(.,.+1)}"
  (| 42 {"a" (& . (+ . 1))}))

(deft t41 "\"b\" | @base64 \"a\\(.)c\""
  (| "b" (cq/str "a" (jq/jq-format :base64) "c")))

(deft t40 "[1,2],[\"h\",\"i\"],[[3,4],[5,6]] | .[0]+.[1]"
  (| (& [1 2]
        ["h" "i"]
        [[3 4] [5 6]])
     (jq/jq-+ (get . 0) (get . 1))))

(deft t39 "1 + 2 + 4 + 8 * 16 * 32 - 64 - 128"
  (- (+ 1 2 4 (* 8 16 32)) 64 128))

(deft t38 "5 | [., 10 + .]"
  (letlift [list]
    (| 5 (list . (+ 10 .)))))

(deft t37 "[3,-2,5,1,-3] | .[] | select(. > 0) |= .*2"
  #_(& 6 -2 10 2 -3)
  (| [3 -2 5 1 -3]
     (each .)
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
  (letfn [(addvalue [f] (let [$x f]
                          (| (each .) [(concat . $x)])))]
    (| [[1,2],[10,20]]
       (addvalue (get . 0)))))

(deft t32 "def foo(f): f|f; 5|foo(.*2)"
  (letfn [(foo [f] (| f f))]
    (| 5 (foo (* . 2)))))

(deft t31 "[3,4] | [.[],9] as $a | .[],$a[]"
  (| [3 4]
     (let [$a (collect-into [] (& (each .) 9))]
       (& (each .) (| $a (each .))))))

(deft t30  "(1,2,3) as $a | $a + 1"
  (let [$a (& 1 2 3)]
    (+ $a 1)))

(deft t29 "5 as $a | $a"
  (let [$a 5] $a))

(let [f (fn [p]
          (cq/with-refer-all [cq]
            (& (path p)
               (modify p (+ . 1)))))]
  (deft t28 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
    (| [[5]]
       (f (-> . (get 0) (get 0))))))

(deft t27 "[[7],[8],[9]] | .[] |= ( .[] |= .+1 )"
  (| [[7] [8] [9]]
     (modify (each .)
             (modify (each .)
                     (+ . 1)))))

(deft t26 "[[7],[8],[9]] | (.[] | .[0]) |= .+1"
  (| [[7] [8] [9]]
     (modify (-> . each (get 0))
             (+ . 1))))

(deft t25 "[[7],[8],[9]] | .[] | .[0] |= .+1"
  (| [[7] [8] [9]]
     (each .)
     (modify (get . 0) (+ . 1))))

(deft t24 "[1,2,3] | (.[0,1]) |= .*2"
  (| [1 2 3]
     (modify (get . (& 0 1))
             (* . 2))))

(deft t23 "[4,5,6] | .[0,1]"
  (| [4 5 6] (get . (& 0 1))))

(deft t22 "path(.[0] | .[1] | .[2])"
  (path (-> .
            (get 0)
            (get 1)
            (get 2))))

(deft t21 "[0,[[1]]] | .[1][0][0] |= . + 5"
  (| [0 [[1]]]
     (modify (-> .
                 (get 1)
                 (get 0)
                 (get 0))
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
     (get . (& 1 2))))

(deft t16 "[1,2,3],[4,5,6],[7,8,9] | .[.[]]"
  (| (& [1 2 3] [4 5 6] [7 8 9])
     (get . (each .))))

(deft t15 "[1,2,3] | path(.[])"
  (| [1 2 3]
     (path (each .))))

(deft t14 "[1,2,3] | . | .[]"
  (| [1 2 3] . (each .)))

(deft t13 "1 | . | ."
  (| 1 . .))

(deft t12 "1 | first([.])"
  (| 1 (first [.])))

(deft t11 "1,2,3 | [4,.]"
  (| (& 1 2 3)
     (& [4 .])))

(deft t10 "[1,2,3] | first(.[] | . + 1)"
  (| [1 2 3]
     (first (| (each .) (inc .)))))

(deft t9 "[1,2,3] | first(.[])"
  (| [1 2 3]
     (first (each .))))

(deft t8 "[[1],[2],[3]] | .[] | .[0]"
  (| (& [[1] [2] [3]])
     (each .)
     (get . 0)))

(deft t7 "[1,2,3] | .[]"
  (| (& [1 2 3]) (each .)))

(deft t6 "1,2,3 | 99,empty,.+1"
  (| (& 1 2 3)
     (& 99 (&) (+ . 1))))

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
  (| (& 1 (&) 3)
     (+ . 1)))

(deft t1 "[1,2,3] | first"
  (| [1 2 3]
     (get . 0)))

(deft t0 "1,2,3 | . + 1"
  (| (& 1 2 3)
     (+ . 1)))
