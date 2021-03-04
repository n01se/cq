(ns net.n01se.cq.macroish
  (:require [net.n01se.cq.internal :as cqi]
            [net.n01se.cq :as cq]))

(defn combinations
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(defmacro ^:cq/list-args | [& args]
  `(->> ~(first args)
        ~@(map (fn [arg]
                 `(mapcat (fn [~'cq-this] ~arg)))
               (rest args))))

(defn ^:cq/list-args & [& args]
  (apply concat args))

(comment
  ;; goal: make following | macro work with go macro
  ;; steps:
  ;;   1. define | macro
  ;;   2. almost all cq functions take explicit . parameter
  ;;   3. modify becomes a macro that works with . and is magic
  ;;   4. ditto assign
  ;;   5. go* needs to wrap at top level with (as-> nil . ...)

  (defmacro | [& forms]
    `(as-> ~'. ~'. ~@forms))

  (go (| 42 (& . .)))

  (go (| [1 2 3] (modify . (get . 1) #(+ % 10)))) ;=> [1 12 3]

  (go (| [1 2 3] (modify [4 5 6] (get {:whoop :whoop} 1) #(+ % 10)))) ;=> [4 15 6]

  (| (& 1 2 3) (let* [y (& . 10)] (+ y 10))) ;; => (11 20 12 20 13 20)
  (as-> (list 1 2 3) .
    (mapcat (fn [.]
              (let* [y (list . 10)]
                (+ y 10)))
            .))

  :end)

;; same as (cq/apply-stream apply concat .)
(defn ^:cq/list-args ^:cq/nav each [stream-of-colls]
  (mapcat (fn [coll]
            (map-indexed (fn [i _] (cqi/nav-get coll i)) (cqi/navigate coll)))
          stream-of-colls))

(defn ^:cq/nav pick [parent idx]
  (cqi/nav-get parent idx))

(defn ^:cq/list-args apply-stream [fn-stream & args]
  (let [args (map #(map cqi/navigate %) args)
        plain-args (drop-last args)
        last-stream (last args)]
    (mapcat (fn [[f & zargs]]
              (apply f (concat zargs [last-stream])))
            (combinations (cons fn-stream plain-args)))))

(defn ^:cq/list-args my-first [stream]
  (take 1 stream))

(defn ^:cq/list-args collect-into [targets stream]
  (map #(into % stream) targets))

(defn array-map-combinations [& args]
  (map #(apply array-map %) (combinations args)))

(defn expand-form [form]
  (cond
    (seq? form)
    (let [[op & args] form]
      (let [{:keys [cq/list-args cq/nav]} (meta (ns-resolve *ns* op))]
        (if list-args
          `(~op ~@(mapv expand-form args))
          (if-not (next args) ;; make one-arg cases easier to read
            `(map ~op ~(expand-form (first args)))
            `(map #(apply ~op %) (combinations [~@(map expand-form args)]))))))

    (vector? form) `(map vec (combinations (map cqi/navigate [~@(map expand-form form)])))
    (map? form)    `(array-map-combinations ~@(map expand-form (apply concat form)))

    :else
    (case form
      . '(list cq-this)
      [form])))

(defn go* [form]
  `(map cqi/navigate ~(expand-form form)))

(defmacro go [form]
  (go* form))

(comment

  (go* '(| (& [1 2 3] [4 (+ 10 (& 5 100)) 6] [7 8 9])
         (cqi/navigate (cqi/nav-get . (& 1 2)))))

  (go (| (& [1 2 3] [4 (+ 10 (& 5 100)) 6] [7 8 9])
         (cqi/navigate (cqi/nav-get . (& 1 2)))))

  :end)
