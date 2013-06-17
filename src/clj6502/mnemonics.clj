(ns clj6502.mnemonics)

(defn lsb [x] (mod x 256))
(defn msb [x] (quot x 256))

(defn immediate [code x]
  [code (Integer/parseInt (name x))])

(defn absolute 
  ([code x] [code (lsb x) (msb x)])
  ([code code-zero x] (if (< x 256)
                        [code-zero x]
                        [code (lsb x) (msb x)])))

(defn lda
  ([x]
   (cond (number? x) (absolute 173 165 x)
         (keyword? x) (immediate 169 x)
         (vector? x) [161 (first x)]))
  ([x y]
   (cond (and (number? x) (= :x y)) (absolute 189 181 x)
         (and (number? x) (= :y y)) (absolute 185 x)
         (vector? x) [177 (first x)])))

(defn ldx 
  ([x]
   (cond (keyword? x) (immediate 162 x)
             (number? x) (absolute 174 166 x)))
  ([x y]
   (cond (and (number? x) (= :y y)) (absolute 190 182 x))))
