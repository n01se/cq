(ns net.n01se.cq.jq-compiler
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [net.n01se.cq :as cq]))

(def parse (insta/parser
            (slurp (io/resource "net/n01se/cq/jq.grammar"))
            :auto-whitespace :standard))

(defn jq-compile [[node & args]]
  (case node
    :statements (some jq-compile args)
    :number (read-string (first args))
    :invoke (let [ident (-> args first second)
                  args (map jq-compile (next (take-nth 2 args)))]
              (case ident
                "empty" cq/hole
                "first" (if (empty? args)
                          (cq/cq-get 0)
                          (apply cq/cq-first args))
                "path" (apply cq/path args)
                (prn :unknown-ident ident)))
    (if (and (= 1 (count args)) (vector? (first args)))
      (jq-compile (first args))
      (if-let [s (first (filter string? args))]
        (case s
          "|" (apply cq/| (map jq-compile (take-nth 2 args)))
          "," (apply cq/& (map jq-compile (take-nth 2 args)))
          "+" (apply cq/+ (map jq-compile (take-nth 2 args)))
          "[" (mapv jq-compile (take-nth 2 (next args)))
          ".[" (cq/cq-get (jq-compile (second args)))
          "|=" (let [[a _ b] args]
                 (cq/modify (jq-compile a) (jq-compile b)))
          "." cq/.
          ".[]" cq/all
          "def" (let [fname (-> args second second)
                      params (when (= :params (-> args (nth 2) first))
                               (next (take-nth 2 (nth args 2))))
                      expr (last (drop-last args))]
                  (prn :args args)
                  (prn :fname fname)
                  (prn :params params)
                  (prn :expr (cq/emit (jq-compile expr))))
          (prn :unknown-exp-str s))
        (prn :unknown-node (cons node args))))))

#_(cq/emit (jq-compile (parse "1,2,3 | .")))

