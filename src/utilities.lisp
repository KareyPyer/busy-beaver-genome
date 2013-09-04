;;author: Vinnie Monaco
;;date  : December, 2008
;;
;;file  : utilities.lisp
;;contains utilities not specific to any part of the system
;;
;;Part of the Busy Beaver Genome Project
;;for Dr. Benjamin's CS385 Artificial Intelligence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-to-list (number &optional number-list)
  (if (zerop number)
      number-list
      (number-to-list (floor number 10) (cons (mod number 10)
					      number-list))))

(defun list-to-number (number-list &optional (number 0))
  (if (endp number-list)
      number
      (list-to-number (cdr number-list) (+ (* number 10) 
					   (car number-list)))))

(defun convert-bin-to-dec (bin &optional (result 0)  (place 0) )
  (cond
    ((zerop bin)
     result) 
    ((evenp bin)
     (convert-bin-to-dec (/ bin 10) result (+ place 1)))
    (t
     (convert-bin-to-dec (/ (- bin 1) 10) 
			 (+ result (expt 2 place)) 
			 (+ place 1)))))


(defun convert-dec-to-bin (dec &optional (result 0) (power 0))
  (cond
    ((zerop dec)
     result)
    (t
     (convert-dec-to-bin (truncate dec 2) 
			 (+ result (* (expt 10 power) (rem dec 2)))
			 (+ power 1)))))


(defun first-n (lst n &optional (count 0))
  (unless (= n count)
    (cons (car lst) (first-n (cdr lst) n (1+ count)))))
