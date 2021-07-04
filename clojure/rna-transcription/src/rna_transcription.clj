(ns rna-transcription)

(defn get-complement [nuc]
  (cond
    (= nuc \G) \C
    (= nuc \C) \G
    (= nuc \T) \A
    (= nuc \A) \U
    :else (throw (AssertionError. "Not a valid nucleotide"))))

(defn to-rna [dna]
  (apply str (map get-complement (char-array dna))))

