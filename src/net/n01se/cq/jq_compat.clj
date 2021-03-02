(ns net.n01se.cq.jq-compat
  (:require [cheshire.core :as json]
            [clojure.core :as clj]
            [clojure.string :as str]
            [net.n01se.cq.internal :as cqi :refer [mfn]]
            [net.n01se.cq :as cq]))

;; jq-compatible get. ew.
(defn nav-get [parent i]
  (let [i (cqi/navigate i)]
    (cqi/nav-lens parent
                  (reify
                    cqi/ILens
                    (lens-get [_ v]
                      (let [v (if (seq? v) (vec v) v)]
                        (cond
                          (nil? v) (if (number? i)
                                     (cqi/ex-assert (<= 0 i)
                                                    "Out of bounds negative array index")
                                     nil)
                          (vector? v) (if (neg? i)
                                        (get v (+ (count v) i))
                                        (get v i))
                          (associative? v) (v i)
                          :else (throw (Exception.
                                        (str "Illegal lookup on " (type v)))))))
                    (lens-put [_ v newval]
                      (let [v (if (seq? v) (vec v) v)]
                        (cond
                          (vector? v) (if (neg? i)
                                        (assoc v (+ (count v) i) newval)
                                        (assoc v i newval))
                          :else (assoc v i newval))))
                    (lens-chart [_] i)))))

(defn nav-slice [parent start-idx end-idx]
  (let [start-idx (cqi/navigate start-idx)
        end-idx (cqi/navigate end-idx)]
    (cqi/nav-lens parent
                  (reify
                    cqi/ILens
                    (lens-get [_ obj]
                      (cond
                        (nil? obj) nil
                        (string? obj) (subs obj
                                            (max start-idx 0)
                                            (min end-idx (count obj)))
                        (sequential? obj) (->> obj
                                               (drop start-idx)
                                               (take (- end-idx start-idx))
                                               vec)
                        :else (cqi/ex-assert
                               false
                               (str "Bad object type for slice: " (type obj)))))
                    (lens-put [_ obj newval]
                      (cqi/ex-assert (sequential? newval)
                                     (str "modify on slice must return sequential, not "
                                          (type newval) ": " (pr-str newval)))
                      (concat (take start-idx obj)
                              newval
                              (drop (max start-idx end-idx) obj)))
                    (lens-chart [_] {:start start-idx, :end end-idx})))))

(def jq-get (cqi/lift-nav-aware nav-get))
(def slice (cqi/lift-nav-aware nav-slice))

(def ^:publish each
  (cqi/mk-each nav-get))

(def jq-+
  (cqi/lift
   #(cond
      (every? number? %&) (apply clj/+ %&)
      (every? string? %&) (apply str %&)
      :else (apply clj/concat %&))
   {:sym `+}))

(defn csv-str [x]
  (if (string? x)
    (str \" (str/replace x "\"" "\"\"") \")
    (str x)))

(defn tsv-str [x]
  (if (string? x)
    (str/replace x "\t" "\\t")
    (str x)))

(defn jq-format [ftype]
  (mfn mfn-format {:mfc-expr `(format ~ftype)} [x]
       (list
        (case ftype
          :text x
          :json (json/generate-string x)
          :csv (str/join "," (map csv-str x))
          :tsv (str/join "\t" (map tsv-str x))
          :html (apply str (map #(case %
                                   \' "&apos;"
                                   \" "&quot;"
                                   \& "&amp;"
                                   \< "&lt;"
                                   \> "&gt;"
                                   %) x))
          :uri (str/replace x #"[^\w']" (fn [s]
                                          (->> (.getBytes s)
                                               (map #(clj/format "%%%02X" %))
                                               (apply str))))
          :sh (str \' (str/replace x #"'" "'\\\\''") \')
          :base64 (String. (.encode (java.util.Base64/getEncoder) (.getBytes x)) "UTF-8")
          :base64d (String. (.decode (java.util.Base64/getDecoder) x))
          x))))

(def tojson (cqi/lift json/generate-string {:sym `tojson}))
(def fromjson (cqi/lift json/parse-string {:sym `fromjson}))

(defn jq-try [expr & [catch-expr]]
  (mfn mfn-try {:mf-expr (list `try expr)} [x]
       (try (doall (cqi/invoke-value expr x))
            (catch Exception ex
              (if catch-expr
                (cqi/invoke catch-expr (.getMessage ex))
                ())))))

(def jq-tree-seq
  (mfn mfn-tree-seq {:mf-expr `tree-seq} [x]
       (tree-seq coll?
                 #(if (map? %) (vals %) (seq %))
                 x)))

(defn jq-range [& args]
  (mfn mfn-jq-range {:mf-expr `(jq-range ~@args)} [x]
       (->> (cqi/cartesian-product-rev (map #(cqi/invoke-value % x) args))
            (mapcat #(apply range %)))))

(defn jq-while [cont? iter]
  (mfn mfn-jq-while {:mf-expr `(jq-while ~cont? ~iter)} [x]
       (take-while #(cqi/cq-eval1 % cont?)
                   (iterate #(cqi/cq-eval1 % iter) x))))
