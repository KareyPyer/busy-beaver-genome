;;author: Vinnie Monaco
;;date  : December, 2008
;;
;;file  : evolution.lisp
;;contains the genetic algorithm and functions
;;to assist the genetics algorithm
;;
;;Part of the Busy Beaver Genome Project
;;for Dr. Benjamin's CS385 Artificial Intelligence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun run-with-output (results-out-stream &optional fitness-out-stream)
"Runs the genetic algorithm with results output to a file and
fitness results optionally output to a file"

    (setf 
     start-time (get-internal-real-time)
     top-contender (genetic-algorithm (generate-population 
				       *pop-size* 
				       *num-states*)
				      *generations* 
				      #'fitness-test
				      fitness-out-stream)
     stop-time (get-internal-real-time))
    (format results-out-stream (concatenate 'string
		"~%******** Search completed in " 
		(write-to-string (float (/ (- stop-time start-time) 1000))) 
				 " secs ********~%"
		"~%Population size~0,8@T : " (write-to-string *pop-size*)
		"~%Generations~0,8@T : " (write-to-string *generations*)
		"~%Mutation Rate~0,8@T : " (write-to-string *mutation-rate*)
		"~%Machine States~0,8@T : " (write-to-string *num-states*)
		"~%Shift Limit~0,8@T : " (write-to-string *default-shift-limit*)
		"~%Fitness weights: "
		"~%  Weight a~0,4@T : " (write-to-string *weight-a*)
		"~%  Weight b~0,4@T : " (write-to-string *weight-b*)
		"~%  Weight c~0,4@T : " (write-to-string *weight-c*)
		"~%  Weight d~0,4@T : " (write-to-string *weight-d*)
		"~%  Weight e~0,4@T : " (write-to-string *weight-e*)
		"~%  Weight f~0,4@T : " (write-to-string *weight-f*)
		"~%  Weight g~0,4@T : " (write-to-string *weight-g*)
		"~%  Weight h~0,4@T : " (write-to-string *weight-h*)
		"~%  Weight i~0,4@T : " (write-to-string *weight-i*)
		"~%~%**************************************************~%~%"
		"~%******************* best fit *********************~%"
		(basic-machine-info-string  top-contender)
		(fitness-string top-contender)
		"~%**************************************************~%")))


;;the genetic algorithm, returns the best fit machine
;;it runs until enough generations have been produced
;;because it is not certain that an individual will
;;ever become fit enough
(defun genetic-algorithm (population 
			  gens-to-run
			  &optional 
			  fitness 
			  out-stream
			  (gen-count 0))
"Performs a search through natural selection on a population
of Turing Machines"

  (if (= gens-to-run gen-count)
      (progn
	(setf distribution (create-distribution population fitness)
	      best-fit-machine (best-fit distribution))

      (when (not (null out-stream))
	(format out-stream 
      		"~%~%******** Final Population ********")
	(mapcar (lambda (machine) 
		 (format out-stream (concatenate 'string
		 "~%" (write-to-string (car machine))
		 "~0,8@T" (write-to-string  (second machine))
		 "~2,10@T" (write-to-string (third machine)))))
	       distribution))

      (return-from genetic-algorithm best-fit-machine)))

  (progn
    (setf new-population nil
	  distribution (create-distribution population fitness))
    
    (when (not (null out-stream))
      (format out-stream (concatenate 'string
	      "~%~%**********************************************"
	      "~%Generation : " (write-to-string gen-count)))
      (mapcar (lambda (machine) 
		(format out-stream (concatenate 'string
			"~%" (write-to-string (car machine))
			"~4,8@T" (write-to-string  (second machine))
			"~4,10@T" (write-to-string (third machine)))))
	      distribution))
    
    (dotimes (var (floor (length population) 2) )
      (setf 
       machine-x (random-select distribution)
       machine-y (random-select distribution)
       children (reproduce (car machine-x) (car machine-y))
       new-population (append children new-population))
        
      (if (< (random 1.0) *mutation-rate*)
	  (mutate (first children)))
      (if (< (random 1.0) *mutation-rate*)
	  (mutate (second children))))
    
    (genetic-algorithm (mix new-population)
		       gens-to-run
		       fitness
		       out-stream
		       (+ gen-count 1))))


(defun reproduce (machine-x machine-y)
"Creates two new machines from the machines provided."
  (let* 
      ((length (length machine-x))
       (num-actions (/ length 3))
       (proportion (/ (random num-actions) num-actions)))
    (list  
     (append (first-n machine-x (* proportion length))
	     (last machine-y (* (- 1 proportion) length)))
     (append (last machine-x (* (- 1 proportion) length))
	     (first-n machine-y (* proportion length))))))


(defun mutate (machine &optional (states (num-states machine)))
"Randomizes the state changes in a machine"
  (unless (endp machine)
	(if (= (mod (length (cdr machine)) 3) 0)
	    (setf (car machine) (random (1+ states))))
	(mutate (cdr machine) states)))
  

(defun random-select (distribution &optional (pivot (random 1.0)))
"Uses a population and fitness function to create a distribution
table. A machine is then randomly selected from the table,
with the probability of selecting any machine is
fitness-of-a-machine / sum-of-every-unique-machine"
  (setf prob (second (car distribution)))
  (if (> prob pivot)
      (return-from random-select (car distribution))
      (random-select (cdr distribution) pivot)))


(defun create-distribution (population fitness)
"Creates a list of (probability machine) pairs to be used for the
random selection."
  (setf
   distribution (mapcar (lambda (x) 
			  (setf score (car (funcall fitness x)))
			  (list x score (float score)))
			population)
   max-fitness (eval (cons 'max (mapcar #'second distribution))))

  (mapcar (lambda (x) (setf (second x) (/ (second x) max-fitness)))
	  distribution)
  distribution)

(defun mix (population) 
"Shuffles the population list to ensure equal opportunity"
   (loop 
      :with vector = (coerce population 'vector) 
      :for i :from (1- (length vector)) :downto 1 
      :do (rotatef (aref vector i) (aref vector (random i))) 
      :finally (return (coerce vector (type-of population)))))


(defun best-fit (distribution
		 &optional 
		 (best-machine (caar distribution))
		 (best-machine-score (third (car distribution))))
"Returns the item in the distribution with the highest fitness score"
  (if (endp distribution)
      (return-from best-fit best-machine))
      
      (if (< best-machine-score
	     (third (car distribution)))
	  (best-fit (cdr distribution)
		    (caar distribution) 
		    (third (car distribution)))
	  (best-fit (cdr distribution)
		    best-machine 
		    best-machine-score)))



(defun generate-population (population-size num-states 
			    &optional (num-machine 0))
"Generates a population of n-state random Turing Machines"
  (unless (= population-size num-machine)
    
    (cons (generate-machine num-states)
	   (generate-population population-size 
				num-states
				(+ num-machine 1)))))


(defun generate-machine (num-states &optional (current-state 0))
  (unless (= num-states current-state)
"Generates a random Turing Machine with a binary alphabet according 
to the list representation used. Any Turing Machine generated will 
either halt successfully or not halt."
  
  (list* (random 2) (random 2) (random (+ num-states 1))
	 (random 2) (random 2) (random (+ num-states 1))
	 (generate-machine num-states (+ current-state 1)))))
