(ns net.n01se.cq.take4
  (:require
   [clojure.tools.analyzer :as ana]
   [clojure.tools.analyzer.env :as env]
   [clojure.tools.analyzer.jvm :as ana-jvm]
   [clojure.tools.analyzer.passes :refer [schedule]]
   [clojure.tools.analyzer.passes.emit-form :refer [emit-form]]))

(defn combinations
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(defn ^::stream-aware ^::nav-aware & ;; span
  [args]
  (apply concat args))

(defn analyze [form env]
  (binding [ana/macroexpand-1 (fn [form env] (macroexpand-1 form))
            ana/create-var    (fn [sym env] {:my-var-sym sym})
            ana/parse         ana/-parse
            ana/var?          var?
            env/*env*         (ana-jvm/global-env)]
    (ana/analyze form env)))

;; This works with :walk :pre because that is a different pass when list-stream
;; is :walk :post.  Does `schedule` do things in the wrong order when :walk :post are composed?
(defn mark-invoke-target
  {:pass-info {:walk :pre :depends #{} :after #{}}}
  [{:keys [op] :as ast}]
  (case op
    :invoke (assoc-in ast [:fn :invoke-target] true)

    ;; default
    ast))

;; TODO: use ana/analyze instead of our analyze below:
(defn lift-stream
  "Lift all invokations (expressions) to return streams unless already marked as
  a stream"
  {:pass-info {:walk :post
               :depends #{#'mark-invoke-target}
               :after #{}}}
  [{:keys [op form args] :as ast}]
  (case op

    (:maybe-class :const) (if (:invoke-target ast)
                            ast
                            (analyze (vector form) (ana/empty-env)))
    :invoke (do
              (clojure.pprint/pprint (-> ast :fn))
              (if (-> ast :fn :meta ::stream-aware)
                ast
                (analyze `(map #(apply ~(-> ast :fn :form) %)
                               (combinations ~(mapv :form args)))
                         (ana/empty-env))))

    ;; default:
    ast))

(defn top-analyze [form]
  (-> form
      (analyze (ana/empty-env))
      ((schedule #{#'mark-invoke-target #'lift-stream}))))

(defn cq* [form]
  (emit-form (top-analyze form)))

(defmacro cq [form] (cq* form))

(comment
  (analyze '(& 5 10) (ana/empty-env))
  (emit-form (analyze '(let [x 5] (def x 10) x) (ana/empty-env)))

  (cq* '(+ (& 5 6) 10))

  (top-analyze '(& 5 6))


  (cq (map #(+ 10 %) [4 5 (& 1 2)]))
  ;;=> ((14 15 11) (14 15 12))

  (schedule #{#'mark-invoke-target #'lift-stream} {:debug? true})

  :end)
