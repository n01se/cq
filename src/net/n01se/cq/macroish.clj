(ns net.n01se.cq.macroish)

(defn combinations
  "All the ways to take one item from each sequence"
  [colls]
  (reduce (fn [products coll]
            (for [product products
                  value coll]
              (conj product value)))
          [()]
          (reverse colls)))

(defn go* [form]
  (if-not (seq? form)
    (case form
      . ['cq-this]
      (all each) 'cq-this
      [form])
    (let [[op & args] form]
      (case op
        (& `&) `(vec (apply concat ~(mapv go* args)))
        (| `|) `(->> ~(go* (first args))
                     ~@(map (fn [arg]
                              `(mapcat (fn [~'cq-this] ~(go* arg))))
                            (rest args)))
        jq/first `(take 1 ~(apply go* args))
        ;; default:
        (if-not (next args) ;; make one-arg cases easier to read
          `(map ~op ~(go* (first args)))
          `(map #(apply ~op %) (combinations [~@(map go* args)])))))))

(defmacro go [form]
  (go* form))

(comment

  (go* '(| [1 2 3]
           (jq/first (| all (inc .)))))

  (go (| [1 2 3]
         (jq/first (| all (inc .)))))

  :end)
