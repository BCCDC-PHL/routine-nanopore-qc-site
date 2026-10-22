(ns routine-nanopore-qc.utils)

(defn mean
  "Calculate the artithmetic mean of a collection of numbers"
  [coll]
  (/ (reduce + coll) (count coll)))

(defn round-number
  "Round a number"
  [f]
  (/ (.round js/Math (* 100 f)) 100))

(defn in?
  "true if coll contains elem"
  [coll elem]
  (some #(= elem %) coll))
