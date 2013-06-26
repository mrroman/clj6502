(ns clj6502.mnemonics)

(defn lsb [x] (mod x 256))
(defn msb [x] (quot x 256))

(defn- hex [x] (Integer/parseInt (name x) 16))
(defn immediate [code x]
  [(hex code) (Integer/parseInt (name x))])

(defn absolute 
  ([code x] [(hex code) (lsb x) (msb x)])
  ([code code-zero x] (if (nil? code-zero)
                        (absolute code x)
                        (if (< x 256)
                          [(hex code-zero) x]
                          [(hex code) (lsb x) (msb x)]))))

(def absolute? number?)
(defn absolute-idx? [x y idx]
  (and (number? x) (= idx y)))

(defmacro mnemonic [m-name 
                    & {:keys [immediate zero-page zero-page-x zero-page-y absolute absolute-x absolute-y indirect indirect-x indirect-y accumulator implied]}]
  `(defn ~m-name
     ([]
      [(hex ~implied)])
     ([x#]
      (cond (absolute? x#) (if (nil? ~zero-page)
                             (absolute ~absolute x#)
                             (absolute ~absolute ~zero-page x#))
            (keyword? x#) (if (= x# :a)
                            [(hex ~accumulator)]
                            (immediate ~immediate x#))
            (vector? x#) (if (empty? (rest x#))
                           [(hex ~indirect) (first x#)]
                           [(hex ~indirect-x) (first x#)])))
     ([x# y#]
      (cond (absolute-idx? x# y# :x) (absolute ~absolute-x ~zero-page-x x#)
            (absolute-idx? x# y# :y) (absolute ~absolute-y ~zero-page-y x#)
            (vector? x#) [(hex ~indirect-y) (first x#)]))))

(mnemonic adc
          :immediate :69
          :zero-page :65
          :zero-page-x :75
          :absolute :6D
          :absolute-x :7D
          :absolute-y :79
          :indirect-x :61
          :indirect-y :71
                 )

(mnemonic and
          :immediate :29
          :zero-page :25
          :zero-page-x :35
          :absolute :2D
          :absolute-x :3D
          :absolute-y :39
          :indirect-x :21
          :indirect-y :31
                 )

(mnemonic asl
          :accumulator :0A
          :zero-page :06
          :zero-page-x :16
          :absolute :0E
          :absolute-x :1E
                 )

(mnemonic bit
          :zero-page :24
          :absolute :2C
                 )

(mnemonic bpl :implied :10)

(mnemonic bmi :implied :30)

(mnemonic bvc :implied :50)

(mnemonic bvs :implied :70)

(mnemonic bcc :implied :90)

(mnemonic bcs :implied :B0)

(mnemonic bne :implied :D0)

(mnemonic beq :implied :F0)

(mnemonic brk
          :implied :00
                 )

(mnemonic cmp
          :immediate :C9
          :zero-page :C5
          :zero-page-x :D5
          :absolute :CD
          :absolute-x :DD
          :absolute-y :D9
          :indirect-x :C1
          :indirect-y :D1
                 )

(mnemonic cpx
          :immediate :E0
          :zero-page :E4
          :absolute :EC
                 )

(mnemonic cpy
          :immediate :C0
          :zero-page :C4
          :absolute :CC
                 )

(mnemonic dec
          :zero-page :C6
          :zero-page-x :D6
          :absolute :CE
          :absolute-x :DE
                 )

(mnemonic eor
          :immediate :49
          :zero-page :45
          :zero-page-x :55
          :absolute :4D
          :absolute-x :5D
          :absolute-y :59
          :indirect-x :41
          :indirect-y :51
                 )

(mnemonic clc :implied :18)

(mnemonic sec :implied :38)

(mnemonic cli :implied :58)

(mnemonic sei :implied :78)

(mnemonic clv :implied :B8)

(mnemonic cld :implied :D8)

(mnemonic sed :implied :F8)

(mnemonic inc
          :zero-page :E6
          :zero-page-x :F6
          :absolute :EE
          :absolute-x :FE
                 )

(mnemonic jmp
          :absolute :4C
          :indirect :6C
                 )

(mnemonic jsr
          :absolute :20
                 )

(mnemonic lda
          :immediate :A9
          :zero-page :A5
          :zero-page-x :B5
          :absolute :AD
          :absolute-x :BD
          :absolute-y :B9
          :indirect-x :A1
          :indirect-y :B1
                 )

(mnemonic ldx
          :immediate :A2
          :zero-page :A6
          :zero-page-y :B6
          :absolute :AE
          :absolute-y :BE
                 )

(mnemonic ldy
          :immediate :A0
          :zero-page :A4
          :zero-page-x :B4
          :absolute :AC
          :absolute-x :BC
                 )

(mnemonic lsr
          :accumulator :4A
          :zero-page :46
          :zero-page-x :56
          :absolute :4E
          :absolute-x :5E
                 )

(mnemonic nop
          :implied :EA
                 )

(mnemonic ora
          :immediate :09
          :zero-page :05
          :zero-page-x :15
          :absolute :0D
          :absolute-x :1D
          :absolute-y :19
          :indirect-x :01
          :indirect-y :11
                 )

(mnemonic tax :implied :AA)

(mnemonic txa :implied :8A)

(mnemonic dex :implied :CA)

(mnemonic inx :implied :E8)

(mnemonic tay :implied :A8)

(mnemonic tya :implied :98)

(mnemonic dey :implied :88)

(mnemonic iny :implied :C8)

(mnemonic rol
          :accumulator :2A
          :zero-page :26
          :zero-page-x :36
          :absolute :2E
          :absolute-x :3E
                 )

(mnemonic ror
          :accumulator :6A
          :zero-page :66
          :zero-page-x :76
          :absolute :6E
          :absolute-x :7E
                 )

(mnemonic rti
          :implied :40
                 )

(mnemonic rts
          :implied :60
                 )

(mnemonic sbc
          :immediate :E9
          :zero-page :E5
          :zero-page-x :F5
          :absolute :ED
          :absolute-x :FD
          :absolute-y :F9
          :indirect-x :E1
          :indirect-y :F1
                 )

(mnemonic sta
          :zero-page :85
          :zero-page-x :95
          :absolute :8D
          :absolute-x :9D
          :absolute-y :99
          :indirect-x :81
          :indirect-y :91
                 )

(mnemonic txs :implied :9A)

(mnemonic tsx :implied :BA)

(mnemonic pha :implied :48)

(mnemonic pla :implied :68)

(mnemonic php :implied :08)

(mnemonic plp :implied :28)

(mnemonic stx
          :zero-page :86
          :zero-page-y :96
          :absolute :8E
                 )

(mnemonic sty
          :zero-page :84
          :zero-page-x :94
          :absolute :8C
                 )
