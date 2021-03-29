(ns net.n01se.cq.minimal)

(comment
  "This is a stab at minimally describing cq using just two special forms:
  collect and each along with navigators. Success is achieved when api
  described in macroish.clj is expressed in terms of collect and each plus
  standard clojure functions and macros.")

;; Section 1: primitives

;; Naming proposal: since these two primitives are complements, pick two names
;; that are more complementary.
(defn ^::stream-aware each [arg]
  (apply concat arg))

(defn ^::stream-aware & [& args]
  (each args))

(defn ^::stream-aware collect [& args]
  args)

(defmacro | [& body]
  (if (zero? (count body))
    '.
    `(-> ~(first body)
         ~@(map (fn [segment] 
                  `(^::stream-aware (fn [~'cq-dot] ~(cq* segment))))
                (rest body)))))

(defn combine
  "All the ways to take one item from each sequence"
  [& colls]
  (reduce (fn [combos coll]
            (for [combo combos
                  value coll]
              (conj combo value)))
          [[]]
          colls))

(defn cq* [form]
  (let [form (if (seq? form)
               (macroexpand form)
               form)]
    (cond
      (seq? form)
      (let [op (first form)
            args (rest form)]
        (case op
          let* (let [[bindings & body] args
                     cq-body (map cq* body)]
                 `(let ~bindings ~@cq-body))
          (let [{::keys [stream-aware]} (meta (if (symbol? op)
                                                (ns-resolve *ns* op)
                                                op))  
                cq-args (map cq* args)]
            (cond
              stream-aware `(~op ~@cq-args)
              (every? vector? cq-args) `(~op ~@(each cq-args))
              (= 1 (count cq-args)) `(map ~op ~@cq-args)
              :else `(map (partial apply ~op) (combine ~@cq-args))))))

      (vector? form) (cq* `(vector ~@form))
      (map? form) (cq* `(array-map ~@(apply concat form)))

      (= '. form)
      'cq-dot

      :else
      [form])))

(defmacro cq [& body]
  (cq* `(| nil ~@body)))
