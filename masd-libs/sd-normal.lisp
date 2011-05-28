(defpackage "SD-NORMAL"
  (:use
   #:cl
   #:masd)
  (:export
   #:value->sd-prob
   "STANDARD-NORMAL-PDF"
   "RANDOM-SD"
   "TAIL-AREA-OF-NORMAL-DISTRIBUTION"
   "PROBABILITY-THAT-VALUE-IS-GREATER"))

(in-package "SD-NORMAL")

   
;-------------------------------------------------------------------------------
(defconstant +sqrt-of-two-pi+ (sqrt (* 2.0 pi)))
(defconstant +sapa-sqrt-8-over-e+ (sqrt (/ 8.0d0 (exp 1.0d0))))
(defconstant +sapa-4-time-exp-of-1-over-4+ (* 4.0d0 (exp 0.25d0)))
(defconstant +sapa-4-time-exp-of-minus-1-point-35+ (* 4.0d0 (exp (- 1.35d0))))
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
(defun standard-normal-pdf (x)
  "given x, returns value of standard normal pdf at x"
  (/ (exp (/ (* x x) -2.0)) +sqrt-of-two-pi+))

(defun x-from-pd (p)
  "given pdf value of standard normal, returns the positive x"
  (sqrt (- (* 2 (log (* +sqrt-of-two-pi+ p))))))
  
(defmacro random-sd (&key (mean 0) (sd 1))
  `(+ (* (sdn-random-normal) ,sd) ,mean))

(defun sdn-random-normal ()
  "returns a random deviate from a normal distribution
with zero mean and unit variance"
  (let ((u (random 1.0)))
    (cond
     ((= u 0.0) (sdn-random-normal))   ;bad choice!
     (t
      (let* ((x (/ (* (- (random 1.0) 0.5)
                      +sapa-sqrt-8-over-e+) u))
             (xs (* x x)))
        (cond
         ((<= xs (- 5.0 (* u +sapa-4-time-exp-of-1-over-4+)))
          x)   ;done
         ((>= xs (+ 1.4 (/ +sapa-4-time-exp-of-minus-1-point-35+ u)))
          (sdn-random-normal))   ;do it again
         ((<= xs (- (* 4.0 (log u))))
          x)
         (t
          (sdn-random-normal))))))))

;-------------------------------------------------------------------------------
(defun tail-area-of-normal-distribution
       (q
        &key
        (q-to-infinity-p nil))
  "given
   [1] q (required)
       ==> a quantile
   [2] q-to-infinity-p (keyword; nil)
       ==> if t,   return integral from q to +infinity;
           if nil, return integral from -infinity to q
return
   tail area of the standard norm from
'   either q to +infinity (if q-to-infinity-p is true)
   or     -infinity to q (if q-to-infinity-p is nil)
---
see  Algorithm AS-66, Applied Statistics,
1973, vol. 22, no. 3"
  (if (minusp q)
    (setf q-to-infinity-p (not q-to-infinity-p) 
          q (- q)))
  (cond
   ((or (<= q 7.0d0)
        (and q-to-infinity-p (<= q 18.66d0)))
    (let* ((y (* 0.5 q q))
           (alnorm (if (> q 1.28d0)
                     (* 0.398942280385d0
                        (/ (exp (- y))
                           (+ q -3.8052d-8
                              (/ 1.00000615302d0
                                 (+ q 3.98064794d-4
                                    (/ 1.98615381364d0
                                       (+ q -0.151679116635d0
                                          (/ 5.29330324926d0
                                             (+ q 4.8385912808d0
                                                (/ -15.1508972451d0
                                                   (+ q 0.742380924027d0
                                                      (/ 30.789933034d0 (+ q 3.99019417011d0)))))))))))))
                     (- 0.5
                        (* q
                           (- 0.398942280444d0
                              (* 0.39990348504d0
                                 (/ y
                                    (+ y 5.75885480458d0
                                       (/ -29.8213557807d0
                                          (+ y 2.62433121679d0 (/ 48.6959930692d0 (+ y 5.92885724438d0)))))))))))))
      (if q-to-infinity-p alnorm (- 1.0 alnorm))))
   ;;; too far out in one direction, so return 0.0 or 1.0 ...
   (t
    (if q-to-infinity-p 0.0 1.0))))



;-------------------------------------------------------------------------------
;; generates intervals for 
(defun generate-intervals (start end divisions)
  (loop with d = (/ (- end start) divisions)
       for i from 0 below divisions
       collect
       (list (+ start (* i d)) (- end (* (- divisions (1+ i)) d)))))

(defun generate-probe-points (start end divisions)
  (loop with d = (/ (- end start) (1- divisions))
       for i from 0 below divisions
       collect
       (+ start (* i d))))

(defmacro area-of-interval (start end height)
  `(* (abs (- ,start ,end)) ,height))
(defmacro avg-of-interval (start end)
  `(+ ,start (/ (- ,end ,start) 2)))
(defmacro standardization (x median sd)
  `(/ (- ,x ,median) ,sd))
(defmacro de-standardization (z median sd)
  `(+ (* ,z ,sd) ,median))
 
(defun interval-min (mean1 sd1 mean2 sd2 acc)
  (min (de-standardization (- (x-from-pd acc)) mean1 sd1)
       (de-standardization (- (x-from-pd acc)) mean2 sd2)))
(defun interval-max (mean1 sd1 mean2 sd2 acc)
  (max (de-standardization (x-from-pd acc) mean1 sd1)
       (de-standardization (x-from-pd acc) mean2 sd2)))


;; Given the probability if two sd-norm given the higher chance
(defun probability-that-value-is-greater (mean1 sd1 mean2 sd2 &key (smallest-dens 0.01) (n-div 10))
  (labels ((count-prob (points sum)
	     (if (cdr points)
		 (count-prob (cdr points)
			     (+ sum
				(* (- 1 (tail-area-of-normal-distribution (standardization 
									   (avg-of-interval (car points) (cadr points))
									   mean2
									   sd2)
									   :q-to-infinity-p t))
				   (- (tail-area-of-normal-distribution (standardization
									 (car points)
									 mean1
									 sd1)
									 :q-to-infinity-p t)
				      (tail-area-of-normal-distribution (standardization
									 (cadr points)
									 mean1
									 sd1)
									 :q-to-infinity-p t)))))
		 sum)))

    (count-prob (generate-probe-points (interval-min mean1 sd1 mean2 sd2 smallest-dens)
				       (interval-max mean1 sd1 mean2 sd2 smallest-dens) 
				       n-div)
		0)))





(defun value->sd-prob (value &key (mean 0.0) (sd 1.0))
  (standard-normal-pdf (/ (- value mean) sd)))