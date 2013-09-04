;;author: Vinnie Monaco
;;date  : December, 2008
;;
;;file  : turing-machine.lisp
;;contains functions and parameters to run a Turing Machine
;;each machine is represented as a list of digits which
;;can easily be converted to a number
;;
;;Part of the Busy Beaver Genome Project
;;for Dr. Benjamin's CS385 Artificial Intelligence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *default-tape-size* 30) ;;initial size, tape is dynamic
(defparameter *end-buffer* 4) ;;the buffer between the tape 
                              ;;head and the end of the tape

(defparameter *sample-bb* '(1 1 2  1 0 3 
			    1 0 1  1 1 2 
			    1 0 2  1 1 0))

(defparameter *sample-machine* '(1 1 2  1 0 3 
				 1 0 1  1 1 2 
				 1 0 2  1 1 4 
				 1 0 1  1 0 1))

(defun create-action-table (machine-number)
"Creates a hash map action table from the machine
number"
  (create-action-table-aux 
   (make-hash-table :size (/ (length machine-number) 3))
   machine-number))
  

(defun create-action-table-aux (action-table 
				machine-number 
				&optional (state 1))
"Creates a hash map action table from the machine
number"
  (if (endp machine-number)
      action-table

      (progn
	(setf (gethash (* state 10) action-table) 
	        (first-n machine-number 3)
	      (gethash (+ (* state 10) 1) action-table) 
	        (first-n (nthcdr 3 machine-number) 3))

	(create-action-table-aux action-table 
				 (nthcdr 6 machine-number) 
				 (+ state 1)))))


(defun lookup (state symbol action-table)
  (gethash (+ (* state 10) symbol) action-table))

(defun make-tape (tape-size)
"Creates a tape for the TM"
  (make-array tape-size
	      :element-type 'integer 
	      :initial-element 0 
	      :adjustable t))

(defun execute-machine-aux (action-table shift-limit tape-size)
  "Executes a Turing Machines and returns the results"
  (setf 
   tape-head 0
   current-state 1
   num-shifts 0
   ones-printed 0
   min-neg 0
   max-pos 0
   pos-tape (make-array (/ tape-size 2)
	      :element-type 'integer 
	      :initial-element 0
	      :adjustable t)
   neg-tape (make-array (/ tape-size 2)
	      :element-type 'integer 
	      :initial-element 0
	      :adjustable t))

  ;;main execution of machine
  (loop
     (when (or (= num-shifts shift-limit)
	       (= current-state 0))
       ;;return any important information here
       (return-from execute-machine-aux 
	 '((append (reverse (coerce neg-tape 'list)) 
	           (coerce pos-tape 'list))
	   num-shifts 
	   tape-head
	   ones-printed 
	   length-traversed
	   current-state)))

     ;;lookup the action to take
     (setf 
      action (lookup current-state 
		     (if (< tape-head 0)
			 (aref neg-tape (abs (1+ tape-head)))
			 ;;else
			 (aref pos-tape tape-head)) action-table)
      symbol 
        (first action)
      direction 
        (case (second action) (0 -1) (1 1))
      state 
        (third action))

     ;;adjust the size of a tape if more room is needed
     (if (< (1+ tape-head) (* -1 (- (length neg-tape) *end-buffer*)))
	 (adjust-array neg-tape (* 2 (length neg-tape))))
     (if (> tape-head (- (length pos-tape) *end-buffer*))
	 (adjust-array pos-tape (* 2 (length pos-tape))))

     ;;determine which tape to access (pos or neg) and write the symbol
     (if (< tape-head 0)
	 (setf (aref neg-tape (abs (1+ tape-head))) symbol)
	 ;;else positive tape
	 (setf (aref pos-tape tape-head) symbol))

     ;;change the position of the tape-head, current-state, and num-shifts
     (setf
      tape-head 
        (+ tape-head direction)
      ;;calculate length traversed
      min-neg
        (min min-neg tape-head)
      max-pos
        (max max-pos (1+ tape-head))
      length-traversed
        (+ (abs min-neg) max-pos)
      current-state 
        (third action)
      num-shifts 
        (+ num-shifts 1)
      ones-printed
        (+ ones-printed (first action)))))


(defun execute-machine (machine-number &optional &key
			(tape-size *default-tape-size*)
			(shift-limit *default-shift-limit*))
  (append (execute-machine-aux (create-action-table machine-number)
		       shift-limit tape-size) (list machine-number)))

(defun num-states (machine-number)
  (/ (length machine-number) 6))

;
(defun basic-machine-info-string (machine-number)
"Creates a string of just basic machine information
including the serial number and action table"
  (setf states (/ (length machine-number) 6))
  (format nil
   (concatenate 'string
   "Machine #: " (write-to-string machine-number)
   "~%States: " (write-to-string states)
   "~%~%State~0,8@TRead~0,8@TWrite~0,8@TMove~0,8@TNext-State"
   (action-table-string machine-number)
)))

;; recursively concatenates 
(defun action-table-string (machine-number 
			    &optional 
			    action-string (state 1))
  (if (endp machine-number)
      action-string

      (format nil
     (concatenate 'string
       action-string "~%"
       (write-to-string state) "~0,8@T" 
       (write-to-string 0) "~0,8@T" 
       (write-to-string (car machine-number)) "~0,8@T" ;;write-symbol
       (write-to-string (cadr machine-number)) "~0,8@T" ;;direction
       (write-to-string (caddr machine-number))      ;;next-state
       "~%"
       (write-to-string state) "~0,8@T" 
       (write-to-string 1) "~0,8@T" 
       (write-to-string (car (cdddr machine-number))) "~0,8@T" ;;write-symbol
       (write-to-string (cadr (cdddr machine-number))) "~0,8@T" ;;direction
       (write-to-string (caddr (cdddr machine-number)))      ;;next-state
       (action-table-string (cdddr (cdddr machine-number)) 
			    action-string (+ state 1))))))
       
