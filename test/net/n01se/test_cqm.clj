(ns net.n01se.test-cqm
  (:require [clojure.test :as t]
            [net.n01se.test-cq :as tcq :refer [tests check-jq]]
            [net.n01se.cq.internal :as cqi]
            [net.n01se.cq.macroish :as cqm
             :refer [go go* & | each pick collect path modify]]))

(defn test-cqm [test-key]
  (let [{:keys [jq cq]} (get tests test-key)
        cq-result (eval (cqm/go* cq))]
    (assert (check-jq test-key cq-result jq))
    cq-result))

(defn test-all [tester]
  (doseq [[test-key {:keys [jq cq]}] tests]
    (prn test-key cq)
    (tester test-key))
  :ok)

#_
(defn path-and-modify [p]
  (& (path p)
     (modify p (+ . 1))))
