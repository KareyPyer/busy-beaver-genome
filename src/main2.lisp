;;main.lisp
;;contains functions for setting up and running
;;the genetic algorithm with results output to
;;a file

(defparameter *project-root* ;;change this to 
  "/");;change this to the root of the project dir

(defparameter *results-dir* 
  (concatenate 'string *project-root* "results/"))

(defparameter *files* '("utilities.lisp" 
			"evolution.lisp"
			"fitness.lisp"
			"turing-machine.lisp"))

(defun compile-and-load-all ()
"Compiles and loads all of the files needed"
  (mapcar (lambda (file)
	    (load
		(compile-file
		 (concatenate 'string
			      *project-root*
			      "code/"
			      file))))
	  *files*))

(compile-and-load-all)

;;change these values to adjust the scope of the search


(defparameter *pop-size* 10);;size of the population
(defparameter *num-states* 3);;size of each machine in the population
(defparameter *default-shift-limit* 100);;limit the number of shifts performed
(defparameter *generations* 20);;generations to search for
(defparameter *mutation-rate* 1/10);;mutation ratio
(defparameter *iterations* 20);;number of searches to perform


(defparameter *weight-a* 1/15);;number of shifts performed
(defparameter *weight-b* 1/15);;sigma(n)
(defparameter *weight-c* 1/10);;length of tape traversed
(defparameter *weight-d* 1/15);;max length non-blank symbols
(defparameter *weight-e* 500);;halts/doesn't halt
(defparameter *weight-f* 1/10);;distance of tape-head from start
(defparameter *weight-g* 1/10);;average length of non-blank symbols
(defparameter *weight-h* 100);;number of states used
(defparameter *weight-i* 200);;contains a halt state


(defun run (&optional (times-to-run *iterations*) (iteration 0))
"Runs the genetic algorithm without the fitness results output"
  (when (= times-to-run iteration)
    (return-from run 'done))
  (setf
   results-output-file (concatenate 'string
			    *results-dir*
			    "evolution."
			    (write-to-string iteration)
			    ".txt"))
  (with-open-file (results-out-stream results-output-file 
				      :direction :output
				      :if-exists :supersede)
    (run-with-output results-out-stream))
  (run times-to-run (1+ iteration)))


(defun run-with-fitness-output
"Runs the genetic algorithm with fitness results output
to a file"
    (&optional (times-to-run *iterations*) (iteration 0))
  (when (= times-to-run iteration)
    (return-from run-with-fitness-output 'done))
  (setf
   results-output-file (concatenate 'string
				    *results-dir*
				    "evolution."
				    (write-to-string iteration)
				    ".txt")
   fitness-output-file (concatenate 'string
				    *results-dir*
				    "fitness."
				    (write-to-string iteration)
				    ".txt"))
  (with-open-file (results-out-stream results-output-file 
				      :direction :output
				      :if-exists :supersede)
    (with-open-file (fitness-out-stream fitness-output-file 
					:direction :output
					:if-exists :supersede)
      (run-with-output results-out-stream fitness-out-stream)))
  (run-with-fitness-output times-to-run (1+ iteration)))