(ns net.n01se.cq.jq-compiler
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [net.n01se.cq :as cq]))

(def parse (insta/parser
            (slurp (io/resource "net/n01se/cq/jq.grammar"))
            :auto-whitespace :standard))

(declare jq-compile)

(defn compile-infix-list [args]
  ;; TODO this is not right when a single list contains different ops of the
  ;; same precedence:
  (cons
   (case (first (filter string? args))
     "," `cq/&
     "+" `cq/jq-+
     "-" `cq/-
     "*" `cq/*
     "/" `cq//
     "==" `cq/=
     "!=" `cq/not=
     ">" `cq/>
     "<" `cq/<
     ">=" `cq/>=
     "<=" `cq/<=)
   (map jq-compile (take-nth 2 args))))


(defn jq-compile [[node & args :as arg]]
  (case node
    :statements (some jq-compile args)
    :number (read-string (first args))
    :invoke (let [ident (-> args first second)
                  args (map jq-compile (next args))]
              (case ident
                "empty" `cq/hole
                "first" (if (empty? args)
                          `(cq/cq-get 0)
                          `(cq/cq-first ~@args))
                "path" `(cq/path ~@args)
                "select" `(cq/select ~@args)
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
    :as (prn :as (cons node args))
    (if (and (= 1 (count args)) (vector? (first args)))
      (jq-compile (first args))
      (if-let [s (first (filter string? args))]
        (case s
          "|" (let [parts (take-nth 2 (partition-by #{"|"} args))
                    runs (partition-by second parts)]
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
                        `cq/.
                        (reverse runs)))

          ("," "+" "-" "*" "/" "==" "!=" "<" ">" "<=" ">=")
          (compile-infix-list args)

          "$" (symbol (str "$" (-> args second second)))
          "[" (let [[a b] (map jq-compile (remove string? args))]
                (cond
                  (= "[" (first args)) [a]          ;; build vector
                  (not b) `(cq/| ~a cq/all)         ;; all .[]
                  :else `(cq/| ~a (cq/cq-get ~b)))) ;; navigate
          "|=" (let [[a _ b] args]
                 `(cq/modify ~(jq-compile a) ~(jq-compile b)))
          "." `cq/.
          "if" `(cq/cq-if ~@(map jq-compile (rest args)))
          (throw (ex-info "unknown-exp-str" {:str s})))
        (throw (ex-info (str "unknown-node") {:expr (cons node args)}))))))

#_(jq-compile (parse "1,2,3 | ."))

