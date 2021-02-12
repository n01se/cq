(ns net.n01se.test-cq
  (:require [net.n01se.cq :as cq :refer [.]]
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
      (when-not (= cq jq-out)
        (prn :expected jq-out :got cq))
      (= cq jq-out))))

(defmacro deft [n jq expr]
  `(def ~(vary-meta n assoc
                    :test `(fn []
                             (let [cq# (cq/invoke-value
                                        (cq/with-refer-all [~'cq] ~expr)
                                        nil)]
                               (assert (check-jq cq# ~jq) (str))
                               cq#)))
     (fn [] ((:test (meta (var ~n)))))))

(defn test-here []
  (dorun (map #(%) (keep (comp :test meta val) (ns-publics *ns*))))
  :ok)

;; EXPERIMENTAL dynamically rooted paths

(deft tx3
  "[0],[3,4],[1],[2,5]" ;; is this right?
  #_"def f(p): [p] | path(.[]),(.[] |= .+1); [2,4] | f(.[0,1])"  ;; or this? [0] [1] [3 5]
  (pipe [2 4]
        rooted
        (cq-get (span 0 1))
        (span rooted-path
              (rooted-reset ((lift +) dot 1)))))

(deft tx2 "def f(p): path(p),p |= .+1; [[1]],[[5]] | .[0] | f(.[0])"
  (pipe (span [[1]] [[5]])
        (cq-get 0)
        rooted
        (cq-get 0)
        (span
         rooted-path
         (rooted-reset ((lift +) dot 1)))))

(deft tx1 "def f(p): path(p),p |= .+1; [[1]],[[5]] | f(.[0] | .[0])"
  (pipe (span [[1]] [[5]])
        rooted
        (cq-get 0)
        (cq-get 0)
        (span
         rooted-path
         (rooted-reset ((lift +) dot 1)))))

(deft tx0 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (pipe [[5]]
        rooted
        (cq-get 0)
        (cq-get 0)
        (span
         rooted-path
         (rooted-reset ((lift +) dot 1)))))

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
  (cq-letlift [*]
    (pipe (span 1 2 3)
          (select ((lift not=) (span (* dot 2) dot) 2)))))

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
    (cq-letlift [*]
      (pipe 5 (foo (* dot 2))))))

(deft t31 "[3,4] | [.[],9] as $a | .[],$a[]"
  (pipe [3 4]
        (cq-let [$a [all 9]]
                (span all (pipe $a all)))))

(deft t30  "(1,2,3) as $a | $a + 1"
  (cq-let [$a (span 1 2 3)]
          ((lift +) $a 1)))

(deft t29 "5 as $a | $a"
  (cq-let [$a 5] $a))

(deft t28 "def f(p): path(p),p |= .+1; [[5]] | f(.[0] | .[0])"
  (let [f (fn [p] (span (path p)
                        (modify p ((lift +) dot 1))))]
    (pipe [[5]]
          (f (pipe (cq-get 0) (cq-get 0))))))

(deft t27 "[[7],[8],[9]] | .[] |= ( .[] |= .+1 )"
  (pipe [[7] [8] [9]]
        (modify all
                  (modify all
                            ((lift +) dot 1)))))

(deft t26 "[[7],[8],[9]] | (.[] | .[0]) |= .+1"
  (pipe [[7] [8] [9]]
        (modify (pipe all (cq-get 0))
                  ((lift +) dot 1))))

(deft t25 "[[7],[8],[9]] | .[] | .[0] |= .+1"
  (pipe [[7] [8] [9]]
        all
        (modify (cq-get 0) ((lift +) dot 1))))

(deft t24 "[1,2,3] | (.[0,1]) |= .*2"
  (cq-letlift [*]
    (pipe [1 2 3]
          (modify (cq-get (span 0 1))
                  (* dot 2)))))

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
         ((lift +) dot 5))))

(deft t20 "5 | (1*.,2) - (10*.,20) - (100*.,200)"
  (cq-letlift [* -]
    (pipe 5
          (- (span (* 1 .) 2)
             (span (* 10 .) 20)
             (span (* 100 .) 200)))))

(deft t19 "(1,2) - (10,20) - (100,200)"
  (cq-letlift [-]
    (- (span 1 2)
       (span 10 20)
       (span 100 200))))

(deft t18 "(1,2) + (10,20) + (100,200)"
  ((lift +) (span 1 2)
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
                   ((lift inc) dot)))))

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
        (span 99 hole ((lift +) dot 1))))

(deft t5 "1,2,3 | 99,.+1"
  (pipe (span 1 2 3)
        (span 99 ((lift +) dot 1))))

(deft t4 "1,2,3 | 99,."
  (pipe (span 1 2 3)
        (span 99 dot)))

(deft t3  "1,2,3 | 4,5"
  (pipe (span 1 2 3)
        (span 4 5)))

(deft t2 "1,empty,3 | . + 1"
  (pipe (span 1 hole 3)
        ((lift +) dot 1)))

(deft t1 "[1,2,3] | first"
  (pipe [1 2 3]
        (cq-get 0)))

(deft t0 "1,2,3 | . + 1"
  (pipe (span 1 2 3)
        ((lift +) dot 1)))