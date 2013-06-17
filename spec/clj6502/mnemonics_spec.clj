(ns clj6502.mnemonics-spec
  (:use [clj6502.mnemonics]
        [speclj.core]))

(defn hex [x] (Integer/parseInt (name x) 16))

(defmacro describe-mnemonic [m-func & rules]
  (let [it-rules (map (fn [rule]
                        `(it (str "for " (first ~rule) " returns " (second ~rule))
                             (should= (second ~rule) (apply ~m-func (first ~rule))))) rules)]
    `(describe (:name (meta (var ~m-func)))
               ~@it-rules)))

(describe-mnemonic lda
                   [[0]       [(hex :A5) 0]]
                   [[512]     [(hex :AD) 0 2]]
                   [[710]     [(hex :AD) 198 2]]
                   [[:10]     [(hex :A9) 10]]
                   [[0 :x]    [(hex :B5) 0]]
                   [[128 :x]  [(hex :B5) 128]]
                   [[256 :x]  [(hex :BD) 0 1]]
                   [[[0 :x]]  [(hex :A1) 0]]
                   [[[0] :y]  [(hex :B1) 0]])

(describe-mnemonic ldx
                   [[:10]     [(hex :A2) 10]]
                   [[:12]     [(hex :A2) 12]]
                   [[44]      [(hex :A5) 44]])
