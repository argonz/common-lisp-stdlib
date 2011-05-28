
(defpackage "GENETICS"
  (:use 
   "COMMON-LISP"
   #:masd)

  (:export
;   "CROSSOVER"
   "DIFFERENCE-OF-GENOMS"
   "DISTANCE-GENOM"
   "DISTANCE-OF-GENOMS"
   "ADD-DIFFERENCE-TO-GENOM"
   "ADD-DIFFERENCE-TO-GENOM-BINARY"))

(in-package "GENETICS")


;; macros!! works only with precomputed genoms


;; a headerbol eredetileg atemelt reszek
(defun crossover (genom1 genom2 &optional (ratio 0.5))    
  "barmilyen 2 listat osszerak, de fontos ugyanolyan hosszuak legyenek"
  (loop for i = 0 then (1+ i) repeat (list-length genom1) collecting 
	(nth i (if (< (random 1.0) ratio)
		   genom1
		   genom2))))

;; EZ CSAK EGYENLO HOSSZU GENOMOKKAL MUKODIK --> plasztikus az erdekes azt majd mashogy kell megcsinalni"
(defmacro difference-of-genoms (genom1 genom2)
  (loop for g1 in genom1 
	for g2 in genom2 
	collect (- g1 g2)))

;; szinten csak kozossel mukodik
(defun add-difference-to-genom (genom diff)
  (loop for g1 in genom for d in diff collect (+ g1 d)))

(defun add-difference-to-genom-binary (genom diff)
  (loop for g1 in genom
	for d in diff 
	collect (mod (+ g1 d) 2)))


;; DISTANCE GENOMS

(defun distance-genom (genom1 genom2)
  "genenkenti tavolsag genomot hoz letre"  ;; kesobb ki lehetne - hogy megadhasd milyen funkcioval szeretned
  (loop for g1 in genom1
	for g2 in genom2
	collect (abs (- g1 g2))))

(defun distance-of-genoms (genom1 genom2)
  "sima osszeadassal"
  (loop for d in (distance-genom genom1 genom2) sum d))
