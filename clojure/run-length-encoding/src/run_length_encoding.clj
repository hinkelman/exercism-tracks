(ns run-length-encoding)

(defn rle-same [xs]
  (let [n (count xs)
        N (if (= n 1) "" n)]
    (str N (first xs))))
 
(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (apply str (map rle-same (partition-by identity plain-text))))

(defn digit? [c] (and (>= (compare c \0) 0) 
                      (<= (compare c \9) 0)))

(defn convert-icl [icl letter]
  (let [n (Integer/parseInt (apply str icl))]
    (apply str (repeat n letter))))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (loop [xs (char-array cipher-text)
         icl '()
         acc '()]
    (if (empty? xs)
      (apply str (reverse acc))
      (let [x (first xs)]
        (if (digit? x)
          (recur (rest xs) (cons x icl) acc)
          (if (empty? icl)
            (recur (rest xs) icl (cons (str x) acc))
            (recur (rest xs) '() (cons (convert-icl icl x) acc))))))))
