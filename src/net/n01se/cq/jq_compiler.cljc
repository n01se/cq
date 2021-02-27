(ns net.n01se.cq.jq-compiler
  (:refer-clojure :exclude [compile])
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [net.n01se.cq :as cq :refer [. &]]
            [net.n01se.cq.jq-compat :as jq]))

(def parse (insta/parser
            (slurp (io/resource "net/n01se/cq/jq.grammar"))))

(def op-symbols
  {","  `cq/&
   "+"  `jq/+
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

(declare compile)

(defn compile-infix-list
  "Compile a list of same-precedent infix ops and operands into grouped cq forms."
  [args]
  (->> (rest args)
       (partition 2)
       (partition-by first)
       (reduce (fn [left-form run]
                 (list* (get op-symbols (ffirst run))
                        left-form
                        (map (comp compile second) run)))
               (compile (first args)))))

(defn compile [[node & args :as arg]]
  (case node
    :statements (compile (last args))
    :number (read-string (first args))
    :vector (if-let [v (first args)]
              [(compile v)]
              [])

    :str    `(cq/str ~@(map compile args))
    :istr   (read-string (str \" (first args) \"))
    :format `(jq/format ~(keyword (first args)))
    :format-interp (let [[format-node [_ & parts]] args]
                     `(cq/str ~@(map (fn [[snode :as expr]]
                                          (if (= :istr snode)
                                            (compile expr)
                                            `(cq/| ~(compile expr)
                                                   ~(compile format-node))))
                                        parts)))

    :invoke (let [ident (-> args first second)
                  args (map compile (next args))]
              (case ident
                "true" true
                "false" false
                "null" nil
                "empty" `(cq/&)
                "first" (if (empty? args)
                          `(jq/jq-get 0)
                          `(cq/first ~@args))
                "path" `(cq/path ~@args)
                "select" `(cq/select ~@args)
                "tojson" `(jq/tojson .)
                "fromjson" `(jq/fromjson .)
                ;; Maybe, hopefully, a local?
                (if (empty? args)
                  (symbol ident)
                  `(~(symbol ident) ~@args))))
    :defs (let [defs (->> (drop-last args)
                          (map (fn [d]
                                 (let [[_ [_ ident] [_ & params] fexpr] d]
                                   `(~(symbol ident)
                                     [~@(map #(symbol (get-in % [1 1])) params)]
                                     ~(compile fexpr))))))
                expr (compile (last args))]
            (if (empty? defs)
              expr
              `(cq/letfn [~@defs]
                            ~expr)))

    (let [first-str (first (filter string? args))]
      (cond
        ;; auto-recurse non-terminal rules
        (and (= 1 (count args)) (vector? (first args)))
        (compile (first args))

        ;; in-fix operators
        (get op-symbols first-str)
        (compile-infix-list args)

        :else
        (case first-str
          "." (let [[left _ [ltype :as lnode]] (if (= "." (first args))
                                           (cons nil args)
                                           args)
                    right (if lnode
                            `(jq/jq-get ~(if (= :ident ltype)
                                           (second lnode)
                                           (compile lnode)))
                            `cq/.)]
                (if left
                  `(cq/| ~(compile left) ~right)
                  right))
          "$" (symbol (str "$" (-> args second second)))
          "?" `(jq/try ~(compile (first args)))
          ".." `jq/jq-tree-seq
          "|" (let [parts (take-nth 2 (partition-by (partial = "|") args))
                    runs (reverse (partition-by second (drop-last parts)))]
                (reduce (fn [form run]
                          (if (-> run first second)
                            ;; "as"-run
                            `(cq/let [~@(mapcat
                                            (fn [[expr as]]
                                              [(symbol (str "$" (get-in as [2 1])))
                                               (compile expr)])
                                            run)]
                                        ~form)

                            ;; non-"as"-run
                            `(cq/| ~@(map (comp compile first) run)
                                   ~form)))
                        (compile (first (last parts)))
                        runs))
          "[" (let [[a b c :as vs] (map compile (remove string? args))]
                (case (count vs)
                  1 `(cq/| ~a jq/all) ;; all .[]
                  2 `(cq/| ~a (jq/jq-get ~b))  ;; navigate
                  3 `(cq/| ~a (jq/slice ~b ~c)))) ;; slice
          "{" (into {} (map (fn [[_ [knode :as k] v]]
                              (let [kc (compile k)
                                    kc (if (symbol? kc) (str kc) kc)]
                                (if v
                                  [kc (compile v)]
                                  [kc `(jq/jq-get ~kc)])))
                            (rest args)))
          "if" `(cq/if ~@(map compile (rest args)))
          (throw (ex-info (str "unknown-node " (pr-str (cons node args)))
                          {:entry (cons node args)})))))))

(defn compile-str [jq-str]
  (let [form (try (-> jq-str parse compile)
                  (catch Exception ex
                    (println "parse/compile failed: " jq-str)
                    (throw ex)))]
    (eval form)))

;; Possible simplifications:
;; - flatten nested variatic fns (| a (| b c)) -> (| a b c)
;; - remove unnecessary dots (| . all) -> (| all)
;; - remove one-arg &'s and |'s (| a) -> a

