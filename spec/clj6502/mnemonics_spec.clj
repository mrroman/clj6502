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

(describe-mnemonic adc
                  [[:44]        [(hex :69) 44]]
                  [[44]         [(hex :65) 44]]
                  [[44 :x]      [(hex :75) 44]]
                  [[710]        [(hex :6D) 198 2]]
                  [[710 :x]     [(hex :7D) 198 2]]
                  [[710 :y]     [(hex :79) 198 2]]
                  [[[44 :x]]    [(hex :61) 44]]
                  [[[44] :y]    [(hex :71) 44]]
                 )

(describe-mnemonic and
                  [[:44]        [(hex :29) 44]]
                  [[44]         [(hex :25) 44]]
                  [[44 :x]      [(hex :35) 44]]
                  [[710]        [(hex :2D) 198 2]]
                  [[710 :x]     [(hex :3D) 198 2]]
                  [[710 :y]     [(hex :39) 198 2]]
                  [[[44 :x]]    [(hex :21) 44]]
                  [[[44] :y]   [(hex :31) 44]]
                 )

(describe-mnemonic asl
                  [[:a]         [(hex :0A)]]
                  [[44]         [(hex :06) 44]]
                  [[44 :x]      [(hex :16) 44]]
                  [[710]        [(hex :0E) 198 2]]
                  [[710 :x]     [(hex :1E) 198 2]]
                 )

(describe-mnemonic bit
                  [[44]         [(hex :24) 44]]
                  [[710]        [(hex :2C) 198 2]]
                 )

(describe-mnemonic brk
                  [[]           [(hex :00)]]
                 )

(describe-mnemonic cmp
                  [[:44]        [(hex :C9) 44]]
                  [[44]         [(hex :C5) 44]]
                  [[44 :x]      [(hex :D5) 44]]
                  [[710]        [(hex :CD) 198 2]]
                  [[710 :x]     [(hex :DD) 198 2]]
                  [[710 :y]     [(hex :D9) 198 2]]
                  [[[44 :x]]    [(hex :C1) 44]]
                  [[[44] :y]   [(hex :D1) 44]]
                 )

(describe-mnemonic cpx
                  [[:44]        [(hex :E0) 44]]
                  [[44]         [(hex :E4) 44]]
                  [[710]        [(hex :EC) 198 2]]
                 )

(describe-mnemonic cpy
                  [[:44]        [(hex :C0) 44]]
                  [[44]         [(hex :C4) 44]]
                  [[710]        [(hex :CC) 198 2]]
                 )

(describe-mnemonic dec
                  [[44]         [(hex :C6) 44]]
                  [[44 :x]      [(hex :D6) 44]]
                  [[710]        [(hex :CE) 198 2]]
                  [[710 :x]     [(hex :DE) 198 2]]
                 )

(describe-mnemonic eor
                  [[:44]        [(hex :49) 44]]
                  [[44]         [(hex :45) 44]]
                  [[44 :x]      [(hex :55) 44]]
                  [[710]        [(hex :4D) 198 2]]
                  [[710 :x]     [(hex :5D) 198 2]]
                  [[710 :y]     [(hex :59) 198 2]]
                  [[[44 :x]]    [(hex :41) 44]]
                  [[[44] :y]   [(hex :51) 44]]
                 )

(describe-mnemonic inc
                  [[44]         [(hex :E6) 44]]
                  [[44 :x]      [(hex :F6) 44]]
                  [[710]        [(hex :EE) 198 2]]
                  [[710 :x]     [(hex :FE) 198 2]]
                 )

(describe-mnemonic jmp
                  [[710]        [(hex :4C) 198 2]]
                  [[[44]]      [(hex :6C) 44]]
                 )

(describe-mnemonic jsr
                  [[710]        [(hex :20) 198 2]]
                 )

(describe-mnemonic lda
                  [[:44]        [(hex :A9) 44]]
                  [[44]         [(hex :A5) 44]]
                  [[44 :x]      [(hex :B5) 44]]
                  [[710]        [(hex :AD) 198 2]]
                  [[710 :x]     [(hex :BD) 198 2]]
                  [[710 :y]     [(hex :B9) 198 2]]
                  [[[44 :x]]    [(hex :A1) 44]]
                  [[[44] :y]   [(hex :B1) 44]]
                 )

(describe-mnemonic ldx
                  [[:44]        [(hex :A2) 44]]
                  [[44]         [(hex :A6) 44]]
                  [[710]        [(hex :AE) 198 2]]
                  [[710 :y]     [(hex :BE) 198 2]]
                 )

(describe-mnemonic ldy
                  [[:44]        [(hex :A0) 44]]
                  [[44]         [(hex :A4) 44]]
                  [[44 :x]      [(hex :B4) 44]]
                  [[710]        [(hex :AC) 198 2]]
                  [[710 :x]     [(hex :BC) 198 2]]
                 )

(describe-mnemonic lsr
                  [[:a]         [(hex :4A)]]
                  [[44]         [(hex :46) 44]]
                  [[44 :x]      [(hex :56) 44]]
                  [[710]        [(hex :4E) 198 2]]
                  [[710 :x]     [(hex :5E) 198 2]]
                 )

(describe-mnemonic nop
                  [[]           [(hex :EA)]]
                 )

(describe-mnemonic ora
                  [[:44]        [(hex :09) 44]]
                  [[44]         [(hex :05) 44]]
                  [[44 :x]      [(hex :15) 44]]
                  [[710]        [(hex :0D) 198 2]]
                  [[710 :x]     [(hex :1D) 198 2]]
                  [[710 :y]     [(hex :19) 198 2]]
                  [[[44 :x]]    [(hex :01) 44]]
                  [[[44] :y]   [(hex :11) 44]]
                 )

(describe-mnemonic rol
                  [[:a]         [(hex :2A)]]
                  [[44]         [(hex :26) 44]]
                  [[44 :x]      [(hex :36) 44]]
                  [[710]        [(hex :2E) 198 2]]
                  [[710 :x]     [(hex :3E) 198 2]]
                 )

(describe-mnemonic ror
                  [[:a]         [(hex :6A)]]
                  [[44]         [(hex :66) 44]]
                  [[44 :x]      [(hex :76) 44]]
                  [[710]        [(hex :6E) 198 2]]
                  [[710 :x]     [(hex :7E) 198 2]]
                 )

(describe-mnemonic rti
                  [[]           [(hex :40)]]
                 )

(describe-mnemonic rts
                  [[]           [(hex :60)]]
                 )

(describe-mnemonic sbc
                  [[:44]        [(hex :E9) 44]]
                  [[44]         [(hex :E5) 44]]
                  [[44 :x]      [(hex :F5) 44]]
                  [[710]        [(hex :ED) 198 2]]
                  [[710 :x]     [(hex :FD) 198 2]]
                  [[710 :y]     [(hex :F9) 198 2]]
                  [[[44 :x]]    [(hex :E1) 44]]
                  [[[44] :y]   [(hex :F1) 44]]
                 )

(describe-mnemonic sta
                  [[44]         [(hex :85) 44]]
                  [[44 :x]      [(hex :95) 44]]
                  [[710]        [(hex :8D) 198 2]]
                  [[710 :x]     [(hex :9D) 198 2]]
                  [[710 :y]     [(hex :99) 198 2]]
                  [[[44 :x]]    [(hex :81) 44]]
                  [[[44] :y]   [(hex :91) 44]]
                 )

(describe-mnemonic stx
                  [[44]         [(hex :86) 44]]
                  [[44 :y]      [(hex :96) 44]]
                  [[710]        [(hex :8E) 198 2]]
                 )

(describe-mnemonic sty
                  [[44]         [(hex :84) 44]]
                  [[44 :x]      [(hex :94) 44]]
                  [[710]        [(hex :8C) 198 2]]
                 )

(describe-mnemonic lda
                   [[0]       [(hex :A5) 0]]
                   [[512]     [(hex :AD) 0 2]]
                   [[710]     [(hex :AD) 198 2]]
                   [[:10]     [(hex :A9) 10]]
                   [[0 :x]    [(hex :B5) 0]]
                   [[128 :x]  [(hex :B5) 128]]
                   [[256 :x]  [(hex :BD) 0 1]]
                   [[0 :y]    [(hex :B9) 0 0]]
                   [[[0 :x]]  [(hex :A1) 0]]
                   [[[0] :y]  [(hex :B1) 0]])

(describe-mnemonic ldx
                   [[:10]     [(hex :A2) 10]]
                   [[:12]     [(hex :A2) 12]]
                   [[44]      [(hex :A6) 44]]
                   [[44 :y]   [(hex :B6) 44]]
                   [[513]     [(hex :AE) 1 2]]
                   [[513 :y]  [(hex :BE) 1 2]])

(describe-mnemonic ldy
                   [[:10]     [(hex :A0) 10]]
                   [[:12]     [(hex :A0) 12]]
                   [[44]      [(hex :A4) 44]]
                   [[44 :x]   [(hex :B4) 44]]
                   [[513]     [(hex :AC) 1 2]]
                   [[513 :x]  [(hex :BC) 1 2]])

