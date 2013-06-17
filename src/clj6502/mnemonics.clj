(ns clj6502.mnemonics)

(defn lsb [x] (mod x 256))
(defn msb [x] (quot x 256))

(defn lda 
  ([x]
   (cond (number? x) (if (< x 256) 
                       [165 x]
                       [173 (lsb x) (msb x)])
         (keyword? x) [169 (Integer/parseInt (name x))]
         (vector? x) [161 (first x)]))
  ([x y]
   (cond (number? x) (if (< x 256)
                       [181 x]
                       [189 (lsb x) (msb x)])
         (vector? x) [177 (first x)])))

(defn ldx [x]
  (cond (keyword? x) [162 (Integer/parseInt (name x))]))
