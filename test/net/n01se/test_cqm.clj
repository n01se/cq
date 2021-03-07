(ns net.n01se.test-cqm
  (:require [clojure.test :as t]
            [net.n01se.test-cq :as tcq :refer [tests test-all check-jq]]
            [net.n01se.cq.internal :as cqi]
            [net.n01se.cq.macroish :as cqm
             :refer [go go* & | $ each pick collect path modify expand assign]]))

(defn test-cqm [test-key]
  (let [{:keys [jq cq]} (get tests test-key)
        cq-result (eval (cqm/go* cq))]
    (assert (check-jq test-key cq-result jq))
    cq-result))

(defn ^:cq/stream-aware ^:cq/nav-aware as-stream [streams]
  (first streams))

(defn ^:cq/stream-aware ^:cq/nav-aware path-and-modify [x p]
  (expand
   (let [x (as-stream x)
         p (as-stream p)]
     (| x
        (& (path p)
           (modify p (+ . 1)))))))

#_
(defn-cq path-and-modify [x p]
  (| x
     (& (path p)
        (modify p (+ . 1)))))
