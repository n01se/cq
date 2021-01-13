(ns net.n01se.cq
  (:require [clojure.java.shell :refer [sh]]))

(defonce jq-cache (memoize (fn [jqs] (sh "jq" "-n" jqs))))

(defn check-jq [cq jqs]
  (if-not (string? jqs)
    true
    (let [jq (read-string (str "(" (:out (jq-cache jqs)) ")"))]
      (when-not (= cq jq)
        (prn :expected jq :got cq))
      (= cq jq))))

(defmacro deft [n jq pipe]
  `(def ~(vary-meta n assoc
                    :test `(fn []
                             (let [cq# (~pipe nil)]
                               (assert (check-jq cq# ~jq) (str ))
                               cq#)))
     (fn [] ((:test (meta (var ~n)))))))

(defn test-here []
  (dorun (map #(%) (keep (comp :test meta val) (ns-publics *ns*))))
  :ok)

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

(defn paths [f x]
  "The companion to invoke that returns a sequence of paths"
  )

(defn pipe [& fns]
  (fn [init]
    (reduce (fn [s f] (remove #(= hole %) (mapcat #(invoke f %) s)))
            (remove #(= hole %) (list init))
            fns)))

(defn comma [& exprs]
  (fn [x]
    (mapcat #(invoke % x) exprs)))

(defn my-first [f]
  (fn [x]
    (list (first (invoke f x)))))

(def dot list)
;; .[] seq
;; .   list
;;     (comp list ???)


(deft t12 "1 | first([.])"
  (pipe 1
        (my-first [dot])))

(deft t11 "1,2,3 | [4,.]"
  (pipe (comma 1 2 3)
        (comma [4 dot])))

(deft t10 "[1,2,3] | first(.[] | . + 1)"
  (pipe [1 2 3]
        (my-first (pipe seq (comp list #(+ % 1))))))

(deft t9 "[1,2,3] | first(.[])"
  (pipe [1 2 3]
        (my-first seq)))

(deft t8 "[[1],[2],[3]] | .[] | .[0]"
  (pipe (comma [[1] [2] [3]])
        seq
        (comp list #(nth % 0))))

(deft t7 "[1,2,3] | .[]"
  (pipe (comma [1 2 3]) seq))

(deft t6 "1,2,3 | 99,empty,.+1"
  (pipe (comma 1 2 3)
        (comma 99 hole (comp list inc))))

(deft t5 "1,2,3 | 99,.+1"
  (pipe (comma 1 2 3)
        (comma 99 (comp list inc))))

(deft t4 "1,2,3 | 99,."
  (pipe (comma 1 2 3)
        (comma 99 dot)))

(deft t3  "1,2,3 | 4,5"
  (pipe (comma 1 2 3)
        (comma 4 5)))

(deft t2 "1,empty,3 | . + 1"
  (pipe (comma 1 hole 3)
        (comp list #(+ % 1))))

(deft t1 "[1,2,3] | first"
  (pipe (comma [1 2 3])
        (comp list first)))

(deft t0 "1,2,3 | . + 1"
  (pipe (comma 1 2 3)
        (comp list inc)))
