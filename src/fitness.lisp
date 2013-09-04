;;author: Vinnie Monaco
;;date  : December, 2008
;;
;;file  : fitness.lisp
;;contains the fitness test for the genetic algorithm.
;;it is a compilation of several sub-tests with weights
;;so that tests can easily be added or change. With a
;;good critic, the weights could be adjusted during
;;runtime to simulate a learning behavior
;;
;;Part of the Busy Beaver Genome Project
;;for Dr. Benjamin's CS385 Artificial Intelligence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fitness-test (machine-number)
"Runs several subtests to give a machine a fitness
score"
 (setf machine-info (execute-machine machine-number))
  (cons
   (+ (* *weight-a* (sub-test-a machine-info))
     (* *weight-b* (sub-test-b machine-info))
     (* *weight-c* (sub-test-c machine-info))
     (* *weight-d* (sub-test-d machine-info))
     (* *weight-e* (sub-test-e machine-info))
     (* *weight-f* (sub-test-f machine-info))
     (* *weight-g* (sub-test-g machine-info))
     (* *weight-h* (sub-test-h machine-info))
     (* *weight-i* (sub-test-i machine-info))
     
     )
     machine-info))



(defun haltp (machine-info)
"halting predicate, nil if the machine probably
doesn't halt and t if it definitely halts"
  (if (= (get-last-state machine-info) 0)
      t
      ;;else
      nil))


;;subtests;;

;;
(defun sub-test-a (machine-info)
"number of shifts performed (until the machine stops running,
not necessarily reaching a halt state)"
  (get-num-shifts machine-info))

(defun sub-test-b (machine-info)
"number of non-blank symbols left on the tape = sigma(machine)"
  (sigma machine-info))

(defun sub-test-c (machine-info)
"length of tape traversed"
  (get-length-traversed machine-info))

(defun sub-test-d (machine-info)
"maximum length of any continous block of non-blank symbols left
on the tape"
  (eval (cons 'max (count-ones (get-tape machine-info)))))

(defun sub-test-e (machine-info)
"returns 1 for a machine that halts without error and 0 otherwise"
  (if (= (get-last-state machine-info) 0)
      1
      ;;else
      0))

(defun sub-test-f (machine-info)
"distance of the tape-head from the starting position on
the tape"
  (1- (get-tape-head-position machine-info)))

(defun sub-test-g (machine-info)
"average length of continous non-blank symbols left on the tape"
  (let ((ones-count (count-ones (get-tape machine-info))))
        (/ (eval (cons '+ ones-count))
		  (length ones-count))))


(defun sub-test-h (machine-info)
"number of states used"
  (setf machine-number (get-machine-number machine-info)
	states (num-states machine-number)
	iteration 0
	state-counter 0
	state-list (get-list-of-states machine-number))
  (loop
     (when (> iteration states)
       (return-from sub-test-h state-counter))
     (if (member iteration state-list)
	 (setf state-counter (1+ state-counter)))
     (setf iteration (1+ iteration))))

(defun sub-test-i (machine-info)
"contains a halt state"
  (setf machine-number (get-machine-number machine-info)
	states (get-list-of-states machine-number))
  (if (member 0 states)
      1
      0))
      


;;*********************************************

;;auxiliary functions
(defun get-machine-number (machine-info)
  (seventh machine-info))

(defun get-list-of-states (machine-number &optional state-list)
  (when (<= (length machine-number) 0)
    (return-from get-list-of-states (reverse state-list)))
  (get-list-of-states (cdddr  machine-number)
		      (cons (caddr machine-number) state-list)))

(defun get-tape (machine-info)
  (eval (first machine-info)))

(defun get-num-shifts (machine-info)
  (eval (second machine-info)))

(defun get-tape-head-position (machine-info)
  (eval (third machine-info)))

(defun get-ones-printed (machine-info)
  (eval (fourth machine-info)))

(defun get-length-traversed (machine-info)
  (eval (fifth machine-info)))

(defun get-last-state (machine-info)
  (eval (sixth machine-info)))

(defun sigma (machine-info)
  (count-if (lambda (x) (equal x 1)) (get-tape machine-info)))

(defun count-ones (tape &optional (list-of-counts '()) (current-length 0))
  (if (endp tape)
      (if (zerop current-length)
	  (if (null list-of-counts)
	      '(0)
	  (reverse list-of-counts))
	  (reverse (cons current-length list-of-counts)))
      ;;else
      (if (equal (car tape) 1) 
	  (if (zerop current-length)
	    (count-ones (cdr tape) list-of-counts 1)
	      ;;else
	    (count-ones (cdr tape) list-of-counts (1+ current-length)))
	  ;;else
	  (if (zerop current-length)
	      (count-ones (cdr tape) list-of-counts)
	      ;;else
	      (count-ones (cdr tape) (cons current-length list-of-counts))))))


;;formatting functions
(defun fitness-string (machine-number)
  (setf machine-info (fitness-test machine-number)
	score (first machine-info)
	machine-info (cdr machine-info))
  (format nil
    (concatenate 'string
      "~%~%Fitness Score ~0,10@T: " (write-to-string (float score))	
      "~%Halted      ~0,10@T: " (write-to-string (haltp machine-info))
      "~%Sigma       ~0,10@T: " (write-to-string (sigma machine-info))
      "~%Shifts      ~0,10@T: " 
                   (write-to-string (get-num-shifts machine-info))
      "~%Last State     ~0,10@T: " 
                   (write-to-string (get-last-state machine-info))
      "~%Last Position  ~0,10@T: " 
           (write-to-string (get-tape-head-position machine-info))
      "~%Length Traversed~0,10@T: " 
             (write-to-string (get-length-traversed machine-info))
      "~%Avg Block Size~0,10@T: " 
                   (write-to-string (float (sub-test-g machine-info)))
      "~%Max Block Size~0,10@T: " 
                   (write-to-string (sub-test-d machine-info))
      "~%Condensed Tape: "
                   (write-to-string (count-ones (get-tape machine-info))))))
