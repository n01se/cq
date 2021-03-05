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

(defn ^:cq/stream-aware ^:cq/nav-aware ??mapcat [[f] coll]
  (mapcat f coll))

(defmacro | ;; pipe
  ([] '.)
  ([arg] arg)
  ([arg1 & args]
   `(->> ~arg1
         ~@(map (fn [arg]
                  `(??mapcat (fn [~'cq-this] ~arg)))
                args))))

(defn ^:cq/stream-aware ^:cq/nav-aware & ;; span
  ([] ())
  ([arg] arg)
  ([arg1 & args] (apply concat arg1 args)))

(defn ^:cq/stream-aware ^:cq/nav-aware modify-fn
  [[root] nav-stream [update-fn]]
  (list
   (reduce (fn [acc nav] (cqi/modify* nav acc (comp first update-fn)))
           root nav-stream)))

(defmacro modify [nav update]
  ;; some versions before jq-1.6 use `last` instead of `first`:
  `(modify-fn ~'. ~nav (fn [~'cq-this] ~update)))

;; same as (cq/apply-stream apply concat .)
(defn ^:cq/stream-aware ^:cq/nav-aware each [stream-of-colls]
  (mapcat (fn [coll]
            (map-indexed (fn [i _] (cqi/nav-get coll i)) (cqi/navigate coll)))
          stream-of-colls))

(defn ^:cq/nav-aware pick [parent idx]
  (cqi/nav-get parent idx))

(defn ^:cq/nav-aware path [nav]
  (cqi/chart nav))

(defn ^:cq/stream-aware collect [stream]
  (list (map cqi/navigate stream)))

(defn ^:cq/stream-aware collect-into [targets stream]
  (map #(into % stream) targets))

(defn expand-form [form]
  (let [form (if (seq? form)
               (macroexpand form)
               form)]
    (cond
      (seq? form)
      (let [[op & args] form]
        (case op
          ;; special forms:
          fn* (let [bodies (if (-> args first vector?) (list args) args)]
                `[(fn* ~@(map (fn [[argv & body]] `(~argv ~@(map expand-form body)))
                              bodies))])
          def (let [[sym value] args] `(def ~sym (first ~(expand-form value))))

          ;; default for function invokation
          (let [{:keys [cq/stream-aware cq/nav-aware] :as m} (meta (ns-resolve *ns* op))
                ex-args (mapv expand-form args)
                nav-args (if nav-aware
                           ex-args
                           (mapv (fn [arg-form] `(map cqi/navigate ~arg-form)) ex-args))]
            (if stream-aware
              `(~op ~@nav-args)
              (if-not (next args) ;; make one-arg cases easier to read
                `(map ~op ~(first nav-args))
                `(map #(apply ~op %) (combinations ~nav-args)))))))

      (vector? form) (expand-form `(vector ~@form))
      (map? form)    (expand-form `(array-map ~@(apply concat form)))

      :else
      (case form
        . '(list cq-this)
        [form]))))

(defn go* [form]
  `(let [~'cq-this nil] (map cqi/navigate ~(expand-form form))))

(defmacro go [form]
  (go* form))

(comment

  (go* '(| (& [1 2 3] [4 (+ 10 (& 5 100)) 6] [7 8 9])
         (cqi/navigate (cqi/nav-get . (& 1 2)))))

  (go (| (& [1 2 3] [4 (+ 10 (& 5 100)) 6] [7 8 9])
         (cqi/navigate (cqi/nav-get . (& 1 2)))))

  :end)

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
