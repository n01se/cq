(ns net.n01se.cq.take5
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.tools.analyzer.ast :as ast]
    [clojure.tools.analyzer.jvm :as ana]
    [clojure.tools.analyzer.passes.jvm.emit-form :as emi]))

;; Generics
(defn ^:private combinations
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          colls))

;; Specifics

(def ^:private ^:dynamic collecting false)

(def ^:private analyze ana/analyze)

(def ^:cq each identity)

(defmacro &
  ([] `(each []))
  ([x] x)
  ([x & xs] `(each [~x ~@xs])))

(defn ^:private cq? [ast]
  (doto
    (assoc ast
           :cq (or (-> ast :meta :cq)
                   (condp = (:op ast)
                     :do (-> ast :ret :cq)
                     :local (-> ast :env :locals
                                (get (:form ast))
                                (ast/postwalk cq?)
                                :cq)
                     :let (-> ast :body :cq)
                     :fn-method (-> ast :body :cq)
                     (->> ast ast/children (some :cq)))))
    (#(when (:cq %) (prn '++ (:op %) (:form %))))))

(defn ^:private annotate [ast]
  (ast/postwalk ast cq?))

(defn ^:private transform [ast]
  ast)

(def ^:private emit emi/emit-form)

(defmacro | [])

(defmacro collect [form]
  (if collecting
    `(list ~form)
    (binding [collecting true]
      (-> form
          analyze
          annotate
          transform
          emit))))

;; Uses

(defn use10 []
  (macroexpand
    `(collect (let [v1 42
                    v2 (& 1 2)
                    f1 (fn [x y] (+ x y v1))
                    f2 (fn [x y] (+ x y v2))]
                (f1 v1 v2)))))

(defn use3 []
  (macroexpand
    `(collect (& 1 1))))

(defn use2 []
  (macroexpand
    `(collect (+ 1 1))))

(defn use1 []
  (macroexpand
    `(collect 1)))

