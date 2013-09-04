Vinnie Monaco

December, 2008

Part of the Busy Beaver Genome Project
for Dr. Benjamin's CS385 Artificial Intelligence

# Quickstart

All of the code should be Common Lisp compatible. It was built and
tested with SBCL.

1. Change the *project-root* global parameter to the root of the 
	project.

2. Make sure the results directory exists in the project root.

3. Edit the run time variables in main.lisp to change:
	-the size of the population
	-number of generations
	-mutation rate
	-machine states to search
	-shift limit to run any machine to
	-weights for the fitness function
	-number of searches to perform
	
	They will all already be defined. Change them to alter the search.
	
4. Compile and load main.lisp. This will compile and load every other
	file needed automatically, as long at the *project-root* is correct.
	
5. Run the function (run) to run the algorithm with standard output or 
	(run-with-fitness-output) to run the algorithm with the fitness
	scores of each generation output to the results directory.
	
6. The function will return 'DONE when complete. Check the results
	directory for the evolutionary and fitness score results of each
	search performed. Those files will be overwritten if another search
	is performed without moving them, so rename or move anything worth
	keeping.
