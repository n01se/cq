(ns net.n01se.cq
  (:require net.n01se.cq.internal :reload))

;; Populate this namespace with vars from internal that are tagged :publish
(let [clj (the-ns 'clojure.core)
      this-ns-map (ns-map *ns*)]
  (doseq [[src-sym the-var] (ns-publics 'net.n01se.cq.internal)]
    (when-let [publish (:publish (meta the-var))]
      (let [target-sym (if (true? publish) src-sym (symbol (name publish)))]
        (when (= clj (some-> (get this-ns-map target-sym) .ns))
          (ns-unmap *ns* target-sym))
        (intern *ns* (with-meta target-sym (meta the-var)) the-var)))))
