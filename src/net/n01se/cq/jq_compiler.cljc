(ns net.n01se.cq.jq-compiler
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [net.n01se.cq :as cq]))

(def parse (insta/parser
            (slurp (io/resource "net/n01se/cq/jq.grammar"))))

(def op-symbols
  {","  `cq/&
   "+"  `cq/jq-+
   "-"  `cq/-
   "*"  `cq/*
   "/"  `cq//
   "==" `cq/=
   "!=" `cq/not=
   ">"  `cq/>
   "<"  `cq/<
   ">=" `cq/>=
   "<=" `cq/<=
   "="  'cq/assign
   "|=" `cq/modify})

(declare jq-compile)

(defn compile-infix-list
  "Compile a list of same-precedent infix ops and operands into grouped cq forms."
  [args]
  (->> (rest args)
       (partition 2)
       (partition-by first)
       (reduce (fn [left-form run]
                 (list* (get op-symbols (ffirst run))
                        left-form
                        (map (comp jq-compile second) run)))
               (jq-compile (first args)))))

(defn jq-compile [[node & args :as arg]]
  (case node
    :statements (jq-compile (last args))
    :number (read-string (first args))
    :vector (if-let [v (first args)]
              [(jq-compile v)]
              [])

    :str    `(cq/str ~@(map jq-compile args))
    :istr   (read-string (str \" (first args) \"))
    :format `(cq/jq-format ~(keyword (first args)))
    :format-interp (let [[format-node [_ & parts]] args]
                     `(cq/str ~@(map (fn [[snode :as expr]]
                                          (if (= :istr snode)
                                            (jq-compile expr)
                                            `(cq/| ~(jq-compile expr)
                                                   ~(jq-compile format-node))))
                                        parts)))

    :invoke (let [ident (-> args first second)
                  args (map jq-compile (next args))]
              (case ident
                "true" true
                "false" false
                "null" nil
                "empty" `(cq/&)
                "first" (if (empty? args)
                          `(cq/get 0)
                          `(cq/first ~@args))
                "path" `(cq/path ~@args)
                "select" `(cq/select ~@args)
                "tojson" `cq/tojson
                "fromjson" `cq/fromjson
                ;; Maybe, hopefully, a local?
                (if (empty? args)
                  (symbol ident)
                  `(~(symbol ident) ~@args))))
    :defs (let [defs (->> (drop-last args)
                          (map (fn [d]
                                 (let [[_ [_ ident] [_ & params] fexpr] d]
                                   `(~(symbol ident)
                                     [~@(map #(symbol (get-in % [1 1])) params)]
                                     ~(jq-compile fexpr))))))
                expr (jq-compile (last args))]
            (if (empty? defs)
              expr
              `(cq/letfn [~@defs]
                            ~expr)))

    (let [first-str (first (filter string? args))]
      (cond
        ;; auto-recurse non-terminal rules
        (and (= 1 (count args)) (vector? (first args)))
        (jq-compile (first args))

        ;; in-fix operators
        (get op-symbols first-str)
        (compile-infix-list args)

        :else
        (case first-str
          "." (let [[left _ [ltype :as lnode]] (if (= "." (first args))
                                           (cons nil args)
                                           args)
                    right (if lnode
                            `(cq/get ~(if (= :ident ltype)
                                        (second lnode)
                                        (jq-compile lnode)))
                            `cq/.)]
                (if left
                  `(cq/| ~(jq-compile left) ~right)
                  right))
          "$" (symbol (str "$" (-> args second second)))
          "?" `(cq/try ~(jq-compile (first args)))
          ".." `cq/jq-tree-seq
          "|" (let [parts (take-nth 2 (partition-by (partial = "|") args))
                    runs (reverse (partition-by second (drop-last parts)))]
                (reduce (fn [form run]
                          (if (-> run first second)
                            ;; "as"-run
                            `(cq/let [~@(mapcat
                                            (fn [[expr as]]
                                              [(symbol (str "$" (get-in as [2 1])))
                                               (jq-compile expr)])
                                            run)]
                                        ~form)

                            ;; non-"as"-run
                            `(cq/| ~@(map (comp jq-compile first) run)
                                   ~form)))
                        (jq-compile (first (last parts)))
                        runs))
          "[" (let [[a b c :as vs] (map jq-compile (remove string? args))]
                (case (count vs)
                  1 `(cq/| ~a cq/all) ;; all .[]
                  2 `(cq/| ~a (cq/get ~b))  ;; navigate
                  3 `(cq/| ~a (cq/slice ~b ~c)))) ;; slice
          "{" (into {} (map (fn [[_ [knode :as k] v]]
                              (let [kc (jq-compile k)
                                    kc (if (symbol? kc) (str kc) kc)]
                                (if v
                                  [kc (jq-compile v)]
                                  [kc `(cq/get ~kc)])))
                            (rest args)))
          "if" `(cq/if ~@(map jq-compile (rest args)))
          (throw (ex-info (str "unknown-node " (pr-str (cons node args)))
                          {:entry (cons node args)})))))))

;; Possible simplifications:
;; - flatten nested variatic fns (| a (| b c)) -> (| a b c)
;; - remove unnecessary dots (| . all) -> (| all)
;; - remove one-arg &'s and |'s (| a) -> a

