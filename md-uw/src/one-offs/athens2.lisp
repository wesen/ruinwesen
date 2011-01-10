(in-package :md)

(defun patterns ()
  (get-patterns '(:A01 :A02 :A08 :A10 :B02 :B04 :B05 :B07 :B08 :b09 :B10 :B12
		  :C02 :C03 :C05 :C08 :C11 :C15 :D11 :E07 :E09 :E12 :E15 :E16)
		*pats*))

;; august minimal

(setf *pats* (remove-if-not #'(lambda (x) (typep x 'md-pattern)) *import*))

(defun patterns2 ()
  (get-patterns '(:G13 :G15 :H01 :G07 :G09 :G11 :G01 :G03 :G05 :F11 :F13 :F15 :C15 :H12 :C05 :F05 :F09 :H03 :H09 :H11 :H15 :A16) *pats*))

(setf *group* (make-object 'group :name "minimal" :objects (copy-store-objects (patterns2) :recursive t)))

renumbering A16 to C01
renumbering C05 to C02
renumbering C15 to C03
renumbering F05 to C04
renumbering F09 to C05
renumbering F11 to C06
renumbering F13 to C07
renumbering F15 to C08
renumbering G01 to C09
renumbering G03 to C10
renumbering G05 to C11
renumbering G07 to C12
renumbering G09 to C13
renumbering G11 to C14
renumbering G13 to C15
renumbering G15 to C16
renumbering H01 to D01
renumbering H03 to D02
renumbering H09 to D03
renumbering H11 to D04
renumbering H12 to D05
renumbering H15 to D06
(#<MD-PATTERN POS: C01 {117686B1}> #<MD-PATTERN POS: C02 {117CC1B9}>
 #<MD-PATTERN POS: C03 {12391859}> #<MD-PATTERN POS: C04 {11804D99}>
 #<MD-PATTERN POS: C05 {12391C69}> #<MD-PATTERN POS: C06 {117CE4D1}>
 #<MD-PATTERN POS: C07 {11806201}> #<MD-PATTERN POS: C08 {11804CE9}>
 #<MD-PATTERN POS: C09 {117B48D9}> #<MD-PATTERN POS: C10 {118533E1}>
 #<MD-PATTERN POS: C11 {1247CE11}> #<MD-PATTERN POS: C12 {11786C81}>
 #<MD-PATTERN POS: C13 {11786C71}> #<MD-PATTERN POS: C14 {11786C61}>
 #<MD-PATTERN POS: C15 {11786CB1}> #<MD-PATTERN POS: C16 {11786CA1}>
 #<MD-PATTERN POS: D01 {11786C91}> #<MD-PATTERN POS: D02 {13395D11}>
 #<MD-PATTERN POS: D03 {1247CE69}> #<MD-PATTERN POS: D04 {117BBEB1}>
 #<MD-PATTERN POS: D05 {12391869}> #<MD-PATTERN POS: D06 {14E65159}>)