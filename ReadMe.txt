;; ReadMe
;; CSC 291 - Lisp Assignment 2
;; Shuyang Liu
;; March 01, 2017

1. Files 
.
├── Assignment1.lisp 		- contains some functions from previous assignment, used in sliu_src.lisp
├── ReadMe.txt 				- this file
├── sliu_src.lisp 			- the file that contains all the source code for the functions in this assignment
└── sliu_test.lisp 			- the test file that contains all the testing functions 

2. Running Instruction

`(load "sliu_test.lisp")` 

3. Description
	`op`
		- structure that contains three components;
			- action
			- preconditions
			- effects

	`consider-action` 
		- takes one argument of type op, check if its preconditions are all true in the current *beliefs* kb
		- If yes, return its effects
		- Otherwise, return nil

	`feasible-actions`
		- return a list of possible actions that can be performed according to current state of *beliefs*
		- each action is paired with its effects

	`do-action`
		- takes one argument of type op
		- first check if it is feasible, if not, print out a error message and return nil
		- Otherwise, perform the action by storing its action and effects into *beliefs*, make changes in *ep-memory*
		- takes care of negative actions by removing elements from *beliefs*

	`generate-English`
		- return a string in english that describs the givien predicate

	`?`
		- can answer questions 

4. References
https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node155.html
https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node84.html
https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node64.html
https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node22.html
