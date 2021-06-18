(ns net.n01se.monad
  (:require [net.n01se.cq :as cq]))

;; Experiments with monads. Not actually used by cq.

(def identity-monad
  [(fn unit [x] x)
   (fn bind [mx f] (f mx))
   + - 42])

(def list-monad
  [(fn unit [x] [x])
   (fn bind [mx f]
     (->> mx
          (mapcat f)
          vec))
   (fn f [x] [(inc x)])
   (fn g [x] [(- x)])
   42])

(def cq-monad
  [list
   (fn bind [mx f] (mapcat f mx))

   (#'cq/span #'cq/. #'cq/.)
   (#'cq/pipe #'cq/each (#'cq/first #'cq/.))
   [42]])
   
(def maybe-monad
  [(fn unit [x] {:x x})
   (fn bind [mx f]
     (if-let [x (:x mx)]
       (f x)
       mx))

   (fn f [x] {:x nil})
   (fn g [x] {:x (inc x)})

   42])

(def writer-monad
  [(fn unit [x]
     {:x x :log []})
   (fn [{:keys [x log] :as mx} mf]
     (update (mf x) :log #(concat log %)))

   (fn f [x] {:x (inc x) :log [(str "Increment: " x)]})
   (fn g [x] {:x (dec x) :log [(str "Decrement: " x)]})

   42])

(def reader-monad nil)
(def state-monad nil)
(def continuation-monad nil)

(defn monad? [[unit bind f g x]]
  (let [mx (unit x)]
    (assert (= (bind mx f)
               (f x))
            (str "First Mondaic Law Broken:\n"
                 (with-out-str
                   (clojure.pprint/pprint
                     [(bind mx f) (f x)]))))
    (assert (= (bind mx unit)
               mx)
            (str "Second Mondaic Law Broken:\n"
                 (with-out-str
                   (clojure.pprint/pprint
                     [(bind mx unit) mx]))))
    (assert (= (bind mx #(bind (f %) g))
               (bind (bind mx f) g))
            (str "Third Mondaic Law Broken:\n"
                 (with-out-str
                   (clojure.pprint/pprint
                     [(bind mx #(bind (f %) g))
                      (bind (bind mx f) g)]))))
    true))

;; Identity Monad
(defn test-monads []
  (assert (monad? identity-monad))
  (assert (monad? list-monad))
  (assert (monad? maybe-monad))
  (assert (monad? writer-monad))
  true)
