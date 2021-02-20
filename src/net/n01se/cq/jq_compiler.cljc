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

    :str    `(cq/cq-str ~@(map jq-compile args))
    :istr   (read-string (str \" (first args) \"))
    :format `(cq/jq-format ~(keyword (first args)))
    :format-interp (let [[format-node [_ & parts]] args]
                     `(cq/cq-str ~@(map (fn [[snode :as expr]]
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
                "empty" `cq/hole
                "first" (if (empty? args)
                          `(cq/cq-get 0)
                          `(cq/cq-first ~@args))
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
              `(cq/cq-letfn [~@defs]
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
          "." `cq/.
          "$" (symbol (str "$" (-> args second second)))
          "|" (let [parts (take-nth 2 (partition-by (partial = "|") args))
                    runs (reverse (partition-by second (drop-last parts)))]
                (reduce (fn [form run]
                          (if (-> run first second)
                            ;; "as"-run
                            `(cq/cq-let [~@(mapcat
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
          "[" (let [[a b :as vs] (map jq-compile (remove string? args))]
                (cond
                  (empty? vs) []                     ;; build empty vector
                  (= "[" (first args)) [a]           ;; build vector
                  (= 1 (count vs)) `(cq/| ~a cq/all) ;; all .[]
                  :else `(cq/| ~a (cq/cq-get ~b))))  ;; navigate
          "{" (into {} (map (fn [[k v]]
                              [(str (jq-compile k))
                               (jq-compile v)])
                            (partition 2 (rest args))))
          "if" `(cq/cq-if ~@(map jq-compile (rest args)))
          (throw (ex-info (str "unknown-node " (pr-str (cons node args)))
                          {:entry (cons node args)})))))))

