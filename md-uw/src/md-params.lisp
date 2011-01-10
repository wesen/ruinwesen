(in-package :md)

;;; big tables needed for conversions and stuff

(defparameter *param-to-name*
  '((8 :AMD) (9 :AMF) (10 :EQF) (11 :EQG) (12 :FLTF) (13 :FLTW) (14 :FTLQ) (15 :SRR)
    (16 :DIST) (17 :VOL) (18 :PAN) (19 :DEL) (20 :REV) (21 :LFOS) (22 :LFOD) (23 :LFOM)))

(defparameter *track-names*
  '(:01-BD :02-SD :03-HT :04-MT :05-LT :06-CP :07-RS :08-CB :09-CH :10-OH :11-RC
    :12-CC :13-M1 :14-M2 :15-M3 :16-M4))

(defparameter *model-param-names*
  '((:TRX-BD ((0 :PTCH) (1 :DEC) (2 :RAMP) (3 :RDEC) (4 :STRT) (5 :NOIS) (6 :HARM) (7 :CLIP)))
    (:TRX-B2 ((0 :PTCH) (1 :DEC) (2 :RAMP) (3 :HOLD) (4 :TICK) (5 :NOIS) (6 :DIRT) (7 :DIST)))
    (:TRX-SD ((0 :PTCH) (1 :DEC) (2 :BUMP) (3 :BENV) (4 :SNAP) (5 :TONE) (6 :TUNE) (7 :CLIP)))
    (:TRX-XT ((0 :PTCH) (1 :DEC) (2 :RAMP) (3 :RDEC) (4 :DAMP) (5 :DIST) (6 :DTYP)))
    (:TRX-CP ((0 :CLPY) (1 :TONE) (2 :HARD) (3 :RICH) (4 :RATE) (5 :ROOM) (6 :RSIZ) (7 :RTUN)))
    (:TRX-RS ((0 :PTCH) (1 :DEC) (2 :DIST)))
    (:TRX-CB ((0 :PTCH) (2 :DEC) (3 :ENH) (4 :DAMP) (5 :TONE) (6 :BUMP)))
    (:TRX-CH ((0 :GAP) (1 :DEC) (2 :HPF) (3 :LPF) (4 :MTAL)))
    (:TRX-OH ((0 :GAP) (1 :DEC) (2 :HPF) (3 :LPF) (4 :MTAL)))
    (:TRX-CY ((0 :RICH) (1 :DEC) (2 :TOP) (3 :TTUN) (4 :SIZE) (5 :PEAK)))
    (:TRX-MA ((0 :ATT) (1 :SUS) (2 :REV) (3 :DAMP) (4 :RATL) (5 :RTYP) (6 :TONE) (7 :HARD)))
    (:TRX-CL ((0 :PTCH) (1 :DEC) (2 :DUAL) (3 :ENH) (4 :TUNE) (5 :CLIC)))
    (:TRX-XC ((0 :PTCH) (1 :DEC) (2 :RAMP) (3 :RDEC) (4 :DAMPmid) (5 :DIST) (6 :DTYP)))

    (:EFM-BD ((0 :PTCH) (1 :DEC) (2 :RAMP) (3 :RDEC) (4 :MOD) (5 :MFRQ) (6 :MDEC) (7 :MFB)))
    (:EFM-SD ((0 :PTCH) (1 :DEC) (2 :NOIS) (3 :NDEC) (4 :MOD) (5 :MFRQ) (6 :MDEC) (7 :HPF)))
    (:EFM-XT ((0 :PTCH) (1 :DEC) (2 :RAMP) (3 :RDEC) (4 :MOD) (5 :MFRQ) (6 :MDEC) (7 :CLIC)))
    (:EFM-CP ((0 :PTCH) (1 :DEC) (2 :CLPS) (3 :CDEC) (4 :MOD) (5 :MFRQ) (6 :MDEC) (7 :HPF)))
    (:EFM-RS ((0 :PTCH) (1 :DEC) (2 :MOD) (3 :HPF) (4 :SNAR) (5 :SPTC) (6 :SDEC) (7 :SMOD)))
    (:EFM-CB ((0 :PTCH) (1 :DEC) (2 :SNAP) (3 :FB) (4 :MOD) (5 :MFRQ) (6 :MDEC)))
    (:EFM-HH ((0 :PTCH) (1 :DEC) (2 :TREM) (3 :TFRQ) (4 :MOD) (5 :MFRQ) (6 :MDEC) (7 :FB)))
    (:EFM-CY ((0 :PTCH) (1 :DEC) (2 :FB) (3 :HPF) (4 :MOD) (5 :MFRQ) (6 :MDEC)))

    (:E12-BD ((0 :PTCH) (1 :DEC) (2 :SNAP) (3 :SPLN) (4 :START) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-SD ((0 :PTCH) (1 :DEC) (2 :HP) (3 :RING) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-HT ((0 :PTCH) (1 :DEC) (2 :HP) (3 :HPQ) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-LT ((0 :PTCH) (1 :DEC) (2 :HP) (3 :RING) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-CP ((0 :PTCH) (1 :DEC) (2 :HP) (3 :HPQ) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-RS ((0 :PTCH) (1 :DEC) (2 :HP) (3 :RRTL) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-CB ((0 :PTCH) (1 :DEC) (2 :HP) (3 :HPQ) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-CH ((0 :PTCH) (1 :DEC) (2 :HP) (3 :HPQ) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-OH ((0 :PTCH) (1 :DEC) (2 :HP) (3 :STOP) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-RC ((0 :PTCH) (1 :DEC) (2 :HP) (3 :BELL) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-CC ((0 :PTCH) (1 :DEC) (2 :HP) (3 :HPQ) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-BR ((0 :PTCH) (1 :DEC) (2 :HP) (3 :REAL) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-TA ((0 :PTCH) (1 :DEC) (2 :HP) (3 :HPQ) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-TR ((0 :PTCH) (1 :DEC) (2 :HP) (3 :HPQ) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-SH ((0 :PTCH) (1 :DEC) (2 :HP) (3 :SLEW) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))
    (:E12-BC ((0 :PTCH) (1 :DEC) (2 :HP) (3 :BC) (4 :STRT) (5 :RTRG) (6 :RTIM) (7 :BEND)))

    (:P-I-BD ((0 :PTCH) (1 :DEC) (2 :HARD) (3 :HAMR) (4 :TENS) (5 :DAMP)))
    (:P-I-SD ((0 :PTCH) (1 :DEC) (2 :HARD) (3 :TENS) (4 :RVOL) (5 :RDEC) (6 :RING)))
    (:P-I-MT ((0 :PTCH) (1 :DEC) (2 :HARD) (3 :HAMR) (4 :TUNE) (5 :DAMP) (6 :SIZE) (7 :POS)))
    (:P-I-RS ((0 :PTCH) (1 :DEC) (2 :HARD) (3 :RING) (4 :RVOL) (5 :RDEC)))
    (:P-I-ML ((0 :PTCH) (1 :DEC) (2 :HARD) (3 :TENS)))
    (:P-I-MA ((0 :GRNS) (1 :DEC) (2 :GLEN) (4 :SIZE) (5 :HARD))) ;; ???
    (:P-I-HH ((0 :PTCH) (1 :DEC) (2 :CLSN) (3 :RING) (4 :AG) (5 :AU) (6 :BR) (7 :CLOS)))
    (:P-I-RC ((0 :PTCH) (1 :DEC) (2 :HARD) (3 :RING) (4 :AG) (5 :AU) (6 :BR) (7 :GRAB)))
    (:P-I-CC ((0 :PTCH) (1 :DEC) (2 :HARD) (3 :RING) (4 :AG) (5 :AU) (6 :BR) (7 :GRAB)))

    (:GND-SN ((0 :PTCH) (1 :DEC) (2 :RAMP) (3 :RDEC)))
    (:GND-NS ((0 :DEC)))
    (:GND-IM ((0 :UP) (1 :UVAL) (2 :DOWN) (3 :DVAL)))

    ((:INP-GA :INP-GB) ((0 :VOL) (1 :GATE) (2 :ATCK) (3 :HLD) (4 :DEC)))
    ((:INP-FA :INP-FB) ((0 :ALEV) (1 :GATE) (2 :FATK) (3 :FHLD)
			(4 :FDEC) (5 :FDPH) (6 :FFRQ) (7 :FQ)))
    ((:INP-EA :INP-EB) ((0 :AVOL) (1 :AHLD) (2 :ADEC) (3 :FQ)
			(4 :FDPH) (5 :FHLD) (6 :FDEC) (7 :FFRQ)))

    ((:MID-01 :MID-02 :MID-03 :MID-04
       :MID-05 :MID-06 :MID-07 :MID-08
      :MID-09 :MID-10 :MID-11 :MID-12
      :MID-13 :MID-14 :MID-15 :MID-16)
     ((0 :NOTE) (1 :N2) (2 :N3) (3 :LEN) (4 :VEL) (5 :PB) (6 :MW) (7 :AT)
      (8 :CC1D) (9 :CC1V) (10 :CC2D) (11 :CC2V)
      (12 :CC3D) (13 :CC3V) (14 :CC4D) (15 :CC4V)
      (16 :CC5D) (17 :CC5V) (18 :CC6D) (19 :CC6V)
      (20 :PCHG) (21 :LFOS) (22 :LFOD) (23 :LFOM)))

    (:CTR-AL ((0 :SYN1) (1 :SYN2) (2 :SYN3) (3 :SYN4) (4 :SYN5) (5 :SYN6) (6 :SYN7) (7 :SYN8)))
    (:CTR-8P ((0 :P1) (1 :P2) (2 :P3) (3 :P4) (4 :P5) (5 :P6) (6 :P7) (7 :P8)
	      (8 :P1-TRK) (9 :P1-PAR) (10 :P2-TRK) (11 :P2-PAR)
	      (12 :P3-TRK) (13 :P3-PAR) (14 :P4-TRK) (15 :P4-PAR)
	      (16 :P5-TRK) (17 :P5-PAR) (18 :P6-TRK) (19 :P6-PAR)
	      (20 :P7-TRK) (21 :P7-PAR) (22 :P8-TRK) (23 :P8-PAR)))

    ((:ROM-01 :ROM-02 :ROM-03 :ROM-04
      :ROM-05 :ROM-06 :ROM-07 :ROM-08
      :ROM-09 :ROM-10 :ROM-11 :ROM-12
      :ROM-13 :ROM-14 :ROM-15 :ROM-16      
      :ROM-17 :ROM-18 :ROM-19 :ROM-20
      :ROM-21 :ROM-22 :ROM-23 :ROM-24
      :ROM-25 :ROM-26 :ROM-27 :ROM-28
      :ROM-29 :ROM-30 :ROM-31 :ROM-32

      :RAM-P1 :RAM-P2)
     ((0 :PTCH) (1 :DEC) (2 :HOLD) (3 :BRR) (4 :STRT) (5 :END) (6 :RTRG) (7 :RTIM)))

    ((:RAM-R1 :RAM-R2)
     ((0 :MLEV) (1 :MBAL) (2 :ILEV) (3 :IBAL) (4 :CUE1) (5 :CUE2) (6 :LEN) (7 :RATE)))
    ))

(defparameter *pattern-pos-to-name*
  '((0 :A01) (1 :A02) (2 :A03) (3 :A04) (4 :A05) (5 :A06) (6 :A07) (7 :A08)
    (8 :A09)(9 :A10) (10 :A11) (11 :A12) (12 :A13) (13 :A14) (14 :A15) (15 :A16)

    (16 :B01) (17 :B02) (18 :B03) (19 :B04) (20 :B05) (21 :B06) (22 :B07) (23 :B08)
    (24 :B09) (25 :B10) (26 :B11) (27 :B12) (28 :B13) (29 :B14) (30 :B15) (31 :B16)

    (32 :C01) (33 :C02) (34 :C03) (35 :C04) (36 :C05) (37 :C06) (38 :C07) (39 :C08)
    (40 :C09) (41 :C10) (42 :C11) (43 :C12) (44 :C13) (45 :C14) (46 :C15) (47 :C16)

    (48 :D01) (49 :D02) (50 :D03) (51 :D04) (52 :D05) (53 :D06) (54 :D07) (55 :D08)
    (56 :D09) (57 :D10) (58 :D11) (59 :D12) (60 :D13) (61 :D14) (62 :D15) (63 :D16)

    (64 :E01) (65 :E02) (66 :E03) (67 :E04) (68 :E05) (69 :E06) (70 :E07) (71 :E08)
    (72 :E09) (73 :E10) (74 :E11) (75 :E12) (76 :E13) (77 :E14) (78 :E15) (79 :E16)

    (80 :F01) (81 :F02) (82 :F03) (83 :F04) (84 :F05) (85 :F06) (86 :F07) (87 :F08)
    (88 :F09) (89 :F10) (90 :F11) (91 :F12) (92 :F13) (93 :F14) (94 :F15) (95 :F16)

    (96 :G01) (97 :G02) (98 :G03) (99 :G04) (100 :G05) (101 :G06) (102 :G07) (103 :G08)
    (104 :G09)(105 :G10) (106 :G11) (107 :G12) (108 :G13) (109 :G14) (110 :G15)(111 :G16)

    (112 :H01) (113 :H02) (114 :H03) (115 :H04) (116 :H05) (117 :H06) (118 :H07)(119 :H08)
    (120 :H09) (121 :H10) (122 :H11) (123 :H12) (124 :H13) (125 :H14)(126 :H15) (127 :H16)))

(defun doublemap-to-java (map)
  (dolist (parammap map)
    (let* ((model (first parammap))
	   (model-num (machine-num model))
	   (params (second parammap)))
      (if (listp model)
	  (progn
	    (dolist (mod2 model)
	      (let ((model-num (machine-num mod2)))
		(format t "new ObjectMap(new StringMap[] {~%")
		(map-to-java params)
		(format t "}, ~A),~%" model-num)))
	    )
	  (progn
	    (format t "new ObjectMap(new StringMap[] {~%")
	    (map-to-java params)
	    (format t "}, ~A),~%" model-num))))))
      
(defun map-to-java (map)
  (with-output-to-string (s)
    (dolist (elt map)
      (format t "new StringMap(~S, ~A),~%" (symbol-name (second elt)) (first elt)))))

(defparameter *machine-to-name*
  '((0 :GND-EMPTY) (1 :GND-SN) (2 :GND-NS) (3 :GND-IM)

    (16 :TRX-BD) (17 :TRX-SD) (18 :TRX-XT) (19 :TRX-CP) (20 :TRX-RS) (21 :TRX-CB)
    (22 :TRX-CH) (23 :TRX-OH) (24 :TRX-CY) (25 :TRX-MA) (26 :TRX-CL) (27 :TRX-XC)
    (28 :TRX-B2)

    (32 :EFM-BD) (33 :EFM-SD) (34 :EFM-XT) (35 :EFM-CP) (36 :EFM-RS) (37 :EFM-CB)
    (38 :EFM-HH) (39 :EFM-CY)

    (48 :E12-BD) (49 :E12-SD) (50 :E12-HT) (51 :E12-LT) (52 :E12-CP) (53 :E12-RS)
    (54 :E12-CB) (55 :E12-CH) (56 :E12-OH) (57 :E12-RC) (58 :E12-CC) (59 :E12-BR)
    (60 :E12-TA) (61 :E12-TR) (62 :E12-SH) (63 :E12-BC)

    (64 :P-I-BD) (65 :P-I-SD) (66 :P-I-MT) (67 :P-I-ML) (68 :P-I-MA) (69 :P-I-RS)
    (70 :P-I-RC) (71 :P-I-CC) (72 :P-I-HH)

    (80 :INP-GA) (81 :INP-GB) (82 :INP-FA) (83 :INP-FB) (84 :INP-EA) (85 :INP-EB)

    (96 :MID-01) (97 :MID-02) (98 :MID-03) (99 :MID-04) (100 :MID-05) (101 :MID-06)
    (102 :MID-07) (103 :MID-08) (104 :MID-09) (105 :MID-10) (106 :MID-11) (107 :MID-12)
    (108 :MID-13) (109 :MID-14) (110 :MID-15) (111 :MID-16)

    (112 :CTR-AL) (113 :CTR-8P)

    (128 :ROM-01) (129 :ROM-02) (130 :ROM-03) (131 :ROM-04) (132 :ROM-05) (133 :ROM-06) 
    (134 :ROM-07) (135 :ROM-08) (136 :ROM-09) (137 :ROM-10) (138 :ROM-11) (139 :ROM-12) 
    (140 :ROM-13) (141 :ROM-14) (142 :ROM-15) (143 :ROM-16) 

    (144 :ROM-17) (145 :ROM-18) (146 :ROM-19) (147 :ROM-20) (148 :ROM-21) (149 :ROM-22) 
    (150 :ROM-23) (151 :ROM-24) (152 :ROM-25) (153 :ROM-26) (154 :ROM-27) (155 :ROM-28) 
    (156 :ROM-29) (157 :ROM-30) (158 :ROM-31) (159 :ROM-32)

    (160 :RAM-R1) (161 :RAM-R2) (162 :RAM-P1) (163 :RAM-P2)))


(defun tuning-to-java (mapping)
  (dolist (map mapping)
    (format t "{ ~A, ~A}, ~%" (cm:keynum (first map)) (cdr map))))

(defparameter *trx-bd-mapping*
  '((B0 . 1) (c1 . 7) (cs1 . 12) (d1 . 17) (ds1 . 23) (e1 . 28) (f1 . 33)
    (fs1 . 39) (g1 . 44) (gs1 . 49) (a1 . 55) (as1 . 60) (b1 . 66)
    (c2 . 71) (cs2 . 76) (d2 . 82) (ds2 . 87) (e2 . 92) (f2 . 98) (fs2 . 103)
    (g2 . 108) (gs2 . 114) (a2 . 119) (as2 . 124)))

(defparameter *trx-xt-mapping*
  '((d2 . 2) (ds2 . 7) (e2 . 12) (f2 . 18) (fs2 . 23) (g2 . 28) (gs2 . 34) (a2 . 39)
    (as2 . 44) (b2 . 49) (c3 . 55) (cs3 . 60) (d3 . 65) (ds3 . 71) (e3 . 76)
    (f3 . 81) (fs3 . 87) (g3 . 92) (gs3 . 97) (a3 . 103) (as3 . 108) (b3 . 113)
    (c4 . 118) (cs4 . 124)))

(defparameter *trx-xc-mapping*
  '((f2 . 1) (fs2 . 6) (g2 . 11)
    (gs2 . 17) (a2 . 22) (as2 . 27) (b2 . 33) (c3 . 38)
    (cs3 . 43) (d3 . 49) (ds3 . 54) (e3 . 60) (f3 . 65) (fs3 . 70) (g3 . 76)
    (gs3 . 81) (a3 . 86) (as3 . 92) (b3 . 97) (c4 . 102) (cs4 . 108) (d4 . 113)
    (ds4 . 118) (e4 . 124)))

(defparameter *trx-sn-mapping*
  '((f3 . 3) (fs3 . 13) (g3 . 24) (gs3 . 35) (a3 . 45) (as3 . 56) (b3 . 67)
    (c4 . 77) (cs4 . 88) (d4 . 98) (ds4 . 109) (e4 . 120)))

(defparameter *trx-cl-mapping*
  '((b5 . 5) (c6 . 11) (cs6 . 17) (d6 . 23) (ds6 . 29) (e6 . 36) (f6 . 42)
    (fs6 . 48) (g6 . 54) (gs6 . 60) (a6 . 66) (as6 . 72) (b6 . 78) (c7 . 84)
    (cs7 . 91) (d7 . 97) (ds7 . 103) (e7 . 109) (f7 . 115) (fs7 . 121) (g7 . 127)))

(defparameter *efm-bd-mapping*
  '((gs0 . 1) (a0 . 3) (as0 . 6) (b0 . 9) (c1 . 11) (cs1 . 14) (d1 . 17) (ds1 . 19)
    (e1 . 22) (f1 . 25) (fs1 . 27) (g1 . 30) (gs1 . 33) (a1 . 35) (as1 . 38)
    (b1 . 41) (c2 . 43) (cs2 . 46) (d2 . 49) (ds2 . 51) (e2 . 54) (f2 . 57) (fs2 . 59)
    (g2 . 62) (gs2 . 65) (a2 . 67) (as2 . 70) (b2 . 73) (c3 . 75) (cs3 . 78) (d3 . 81)
    (ds3 . 83) (e3 . 86) (f3 . 89) (fs3 . 91) (g3 . 94) (gs3 . 97) (a3 . 99)
    (as3 . 102) (b3 . 105) (c4 . 107) (cs4 . 110) (d4 . 113) (ds4 . 115) (e4 . 118)
    (f4 . 121) (fs4 . 123) (g4 . 126)))

(defparameter *efm-xt-mapping*
  '((f1 . 1) (fs1 . 7) (g1 . 12) (gs1 . 17) (a1 . 23) (as1 . 28) (b1 . 33)
    (c2 . 39) (cs2 . 44) (d2 . 49) (ds2 . 55) (e2 . 60) (f2 . 65) (fs2 . 71)
    (g2 . 76) (gs2 . 81) (a2 . 87) (as2 . 92) (b2 . 97) (c3 . 102) (cs3 . 108)
    (d3 . 113) (ds3 . 118) (e3 . 124)))

(defparameter *efm-sd-mapping*
  '((b2 . 1) (c3 . 5) (cs3 . 9) (d3 . 14) (ds3 . 18) (e3 . 22) (f3 . 27) (fs3 . 31)
    (g3 . 35) (gs3 . 39) (a3 . 44) (as3 . 48) (b3 . 52) (c4 . 56) (cs4 . 61) (d4 . 65)
    (ds4 . 69) (e4 . 73) (f4 . 78) (fs4 . 82) (g4 . 86) (gs4 . 91) (a4 . 95) (as4 . 99)
    (b4 . 103) (c5 . 108) (cs5 . 112) (d5 . 116) (ds5 . 120) (e5 . 125)))

(defparameter *efm-cp-mapping*
  '((b2 . 0) (c3 . 2) (cs3 . 4) (d3 . 6) (ds3 . 8) (e3 . 10) (f3 . 12) (fs3 . 14) (g3 . 16)
    (gs3 . 18) (a3 . 20) (as3 . 22) (b3 . 24) (c4 . 26) (cs4 . 28) (d4 . 29) (ds4 . 31)
    (e4 . 33) (f4 . 35) (fs4 . 37) (g4 . 39) (gs4 . 41) (a4 . 43) (as4 . 45) (b4 . 47)
    (c5 . 49) (cs5 . 51) (d5 . 53) (ds5 . 55) (e5 . 57) (f5 . 59) (fs5 . 61) (g5 . 62)
    (gs5 . 64) (a5 . 66) (as5 . 68) (b5 . 70) (c6 . 72) (cs6 . 74) (d6 . 76) (ds6 . 78)
    (e6 . 80) (f6 . 82) (fs6 . 84) (g6 . 86) (gs6 . 88) (a6 . 90) (as6 . 92) (b6 . 94)
    (c7 . 95) (cs7 . 97) (d7 . 99) (ds7 . 101) (e7 . 103) (f7 . 105) (fs7 . 107) (g7 . 109)
    (gs7 . 111) (a7 . 113) (as7 . 115) (b7 . 117) (c8 . 119) (cs8 . 121) (d8 . 123) (ds8 . 125)
    (e8 . 127)))

(defparameter *efm-hh-mapping*
  '((b3 . 1) (c4 . 5) (cs4 . 9) (d4 . 14) (ds4 . 18) (e4 . 22) (f4 . 27) (fs4 . 31)
    (g4 . 35) (gs4 . 39) (a4 . 44) (as4 . 48) (b4 . 52) (c5 . 56) (cs5 . 61) (d5 . 65)
    (ds5 . 69) (e5 . 73) (f5 . 78) (fs5 . 82) (g5 . 86) (gs5 . 91) (a5 . 95) (as5 . 99)
    (b5 . 103) (c6 . 108) (cs6 . 112) (d6 . 116) (ds6 . 120) (e6 . 125)))

(defparameter *efm-rs-mapping*
  '((b3 . 1) (c4 . 3) (cs4 . 6) (d4 . 9) (ds4 . 11) (e4 . 14) (f4 . 17) (fs4 . 19)
    (g4 . 22) (gs4 . 25) (a4 . 27) (as4 . 30) (b4 . 33) (c5 . 35) (cs5 . 38) (d5 . 41)
    (ds5 . 43) (e5 . 46) (f5 . 49) (fs5 . 51) (g5 . 54) (gs5 . 57) (a5 . 59) (as5 . 62)
    (b5 . 65) (c6 . 67) (cs6 . 70) (d6 . 73) (ds6 . 75) (e6 . 78) (f6 . 81) (fs6 . 83)
    (g6 . 86) (gs6 . 89) (a6 . 91) (as6 . 94) (b6 . 97) (c7 . 99) (cs7 . 102) (d7 . 105)
    (ds7 . 107) (e7 . 110) (f7 . 113) (fs7 . 115) (g7 . 118) (gs7 . 121) (a7 . 123) (as7 . 126)))

(defparameter *rom-mapping*
  '((a2 . 0)
    (as2 . 2)
    (b2 . 5)
    (c3 . 7)
    (cs3 . 9)
    (d3 . 12)
    (ds3 . 14)
    (e3 . 16)
    (f3 . 19)
    (fs3 . 21)
    (g3 . 23)
    (gs3 . 26)
    (a3 . 28)
    (as3 . 31)
    (b3 . 34)
    (c4 . 37)
    (cs4 . 40)
    (d4 . 43)
    (ds4 . 46)
    (e4 . 49)
    (f4 . 52)
    (fs4 . 55)
    (g4 . 58)
    (gs4 . 61)
    (a4 . 64)
    (as4 . 67)
    (b4 . 70)
    (c5 . 73)
    (cs5 . 76)
    (d5 . 79)
    (ds5 . 82)
    (e5 . 85)
    (f5 . 88)
    (fs5 . 91)
    (g5 . 94)
    (gs5 . 97)
    (a5 . 100)
    (as5 . 102)
    (b5 . 105)
    (c6 . 107)
    (cs6 . 109)
    (d6  112)
    (ds6 . 114)
    (e6 . 116)
    (f6 . 119)
    (fs6 . 121)
    (g6 . 123)
    (gs6 . 125)))

(defun tuning-map-to-java (mappings)
  (dolist (map mappings)
    (let* ((model (first map))
	   (num (machine-num model)))
      (format t "new ObjectMap(new int[][] {~%")
      (tuning-to-java (second map))
      (format t "}, ~A), ~%" num))))

(defparameter *mappings2*
  `((:EFM-RS ,*efm-rs-mapping*)
    (:EFM-HH ,*efm-hh-mapping*)
    (:EFM-CP ,*efm-cp-mapping*)
    (:EFM-SD ,*efm-sd-mapping*)
    (:EFM-XT ,*efm-xt-mapping*)
    (:EFM-BD ,*efm-bd-mapping*)
    (:TRX-CL ,*trx-cl-mapping*)
    (:TRX-SD ,*trx-sn-mapping*)
    (:TRX-XC ,*trx-xc-mapping*)
    (:TRX-XT ,*trx-xt-mapping*)
    (:TRX-BD ,*trx-bd-mapping*)))

(defparameter *mappings*
  `((:EFM_RS ,*efm-rs-mapping*)
    (:EFM_HH ,*efm-hh-mapping*)
    (:EFM_CP ,*efm-cp-mapping*)
    (:EFM_SD ,*efm-sd-mapping*)
    (:EFM_XT ,*efm-xt-mapping*)
    (:EFM_BD ,*efm-bd-mapping*)
    (:TRX_CL ,*trx-cl-mapping*)
    (:TRX_SN ,*trx-sn-mapping*)
    (:TRX_XC ,*trx-xc-mapping*)
    (:TRX_XT ,*trx-xt-mapping*)
    (:TRX_BD ,*trx-bd-mapping*)))

(defun mappings-to-applescript ()
  (with-output-to-string (s)
    (let ((bla (loop for (kword mappings) in *mappings*
		  for name = (string-downcase (symbol-name kword))
		  for basenote = (keynum (caar mappings))
		  collect (list name basenote)
		  do (format s "property ~a_notes: { " name)
		    (format s "~a" (cdar mappings))
		    (loop for (note . val) in (cdr mappings)
		       do (format s ", ~a" val))
		    (format s " }~%"))))
      (format s "property md_mappings : { ~a: {basenote: ~A, pitch: ~a_notes}"
	      (caar bla) (cdar bla) (caar bla))
      (loop for (name basenote) in (cdr bla)
	   do (format s ", ~a: {basenote: ~A, pitch: ~A_notes}" name basenote name))
      (format s " }~%"))))
