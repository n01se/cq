(ns net.n01se.cq.macroish
  (:require [net.n01se.cq.internal :as cqi]))

(defn combinations
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(defn ^:cq/list-args & [& args]
  (apply concat args))

(defmacro ^:cq/list-args | [& args]
  `(->> ~(first args)
        ~@(map (fn [arg]
                 `(mapcat (fn [~'cq-this] ~arg)))
               (rest args))))

(defn ^:cq/list-args my-first [stream]
  (take 1 stream))

(defn ^:cq/list-args collect-into [targets stream]
  (map #(into % stream) targets))

(defn go* [form]
  (cond
    (seq? form)
    (let [[op & args] form]
      (let [{:keys [cq/list-args]} (meta (ns-resolve *ns* op))]
        (if list-args
          `(~op ~@(mapv go* args))
          (if-not (next args) ;; make one-arg cases easier to read
            `(map ~op ~(go* (first args)))
            `(map #(apply ~op %) (combinations [~@(map go* args)]))))))

    (vector? form)
    `(map vec (combinations [~@(map go* form)]))

    :else
    (case form
      . ['cq-this]
      (all each) 'cq-this
      [form])))

(defmacro go [form]
  (go* form))

(comment

  (go* '(| [1 2 3]
           (my-first (| all (inc .)))))

  (go (| (& [1 2 3] [4 5 6] [7 8 9])
         (cqi/nav-get . (& 1 2))))

  :end)
