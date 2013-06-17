(ns clj6502.mnemonics-spec
  (:use [clj6502.mnemonics]
        [speclj.core]))

(defn hex [x] (Integer/parseInt (name x) 16))

(describe "lda"
          (for [item [[[0] [(hex :A5) 0]]
                      [[512] [(hex :AD) 0 2]]
                      [[710] [(hex :AD) 198 2]]
                      [[:10] [(hex :A9) 10]]
                      [[0 :x] [(hex :B5) 0]]
                      [[128 :x] [(hex :B5) 128]]
                      [[256 :x] [(hex :BD) 0 1]]
                      [[[0 :x]] [(hex :A1) 0]]
                      [[[0] :y] [(hex :B1) 0]]]]
            (it (str "for " (first item) " returns " (second item))
                (should= (second item) (apply lda (first item))))))

