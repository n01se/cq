(ns net.n01se.cq.jq-compat
  (:refer-clojure :exclude [+ format try])
  (:require [cheshire.core :as json]
            [clojure.core :as clj]
            [clojure.string :as str]
            [net.n01se.cq.internal :as cqi :refer [mfn]]))

;; jq-compatible get. ew.
(defn nav-get [parent i]
  (cqi/nav-lens parent
                (reify
                  cqi/ILens
                  (lens-get [_ v]
                    (cond
                      (nil? v) nil
                      (seq? v) (nth v i)
                      (associative? v) (v i)
                      :else (throw (Exception.
                                    (str "Illegal lookup on " (type v))))))
                  (lens-put [_ obj newval] (assoc obj i newval))
                  (lens-chart [_] i))))

(defn jq-get [mf]
  (mfn mfn-get {:mfc-expr `(jq-get ~mf)} [x]
   (let [path-elems (cqi/invoke-value mf x)]
     (map #(nav-get x %) path-elems))))

(defn nav-slice [parent start-idx end-idx]
  (cqi/nav-lens parent
                (reify
                  cqi/ILens
                  (lens-get [_ obj]
                    (cond
                      (nil? obj) nil
                      (string? obj) (subs obj
                                          (max start-idx 0)
                                          (min end-idx (count obj)))
                      (vector? obj) (subvec obj
                                            (min (max start-idx 0) (count obj))
                                            (min end-idx (count obj)))
                      :else (cqi/ex-assert false "Bad object type for slice")))
                  (lens-put [_ obj newval]
                    (cqi/ex-assert (sequential? newval)
                                   (str "modify on slice must return sequential, not "
                                        (type newval) ": " (pr-str newval)))
                    (concat (take start-idx obj)
                            newval
                            (drop (max start-idx end-idx) obj)))
                  (lens-chart [_] {:start start-idx, :end end-idx}))))

(defn slice [start-mf end-mf]
  (mfn mfn-slice {:mfc-expr `(slice ~start-mf ~end-mf)} [x]
       (map (fn [[start-idx end-idx]]
              (nav-slice x start-idx end-idx))
            (cqi/cartesian-product-rev [(cqi/cq-eval x start-mf)
                                        (cqi/cq-eval x end-mf)]))))

(def ^:publish all ;; TODO: rename to `each`?
  (cqi/mk-all nav-get))

(def +
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

(defn format [ftype]
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

(defn try [expr]
  (mfn mfn-try {:mf-expr (list `try expr)} [x]
       (try (doall (cqi/invoke-value expr x))
            (catch Exception ex ()))))

(def jq-tree-seq
  (mfn mfn-tree-seq {:mf-expr `tree-seq} [x]
       (prn :x x)
       (tree-seq coll?
                 #(if (map? %) (vals %) (seq %))
                 x)))
