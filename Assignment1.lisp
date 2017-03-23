;; CSC 291 - Lisp Assignment 1
;; Shuyang Liu
;; Feb 13, 2017

;; 1. Saying Hello
; (defparameter *res* "")
; (defun greet ()
; 	"hello world to Common Lisp"
; 	(format t "Hello, how are you?~%")
; 	(setq *res* (read-line))
; 	(if (equal *res* "")
; 		(format t "Well, I guess you are not feeling talkative~%")
; 		(format t "Glad to hear it~%"))
; 	"-------------------------------------")
;; Test
; (format t "Test for part 1: ~%")
; (greet)

;; Assignment 3 1.a Mutually Exclusive Predicates
;; A little different from the instruction 
;; I actually constructed three tables in order to make it seems more clear
;; assume the arguments of this function obeys the rule that predicates from 
;; the same spectrum have the same properties
;; did not include checking mechanism inside (yet)
(defparameter *spectra1*
	(make-hash-table :test 'equal))
(defparameter *spectra2*
	(make-hash-table :test 'equal))
(defparameter *spectra3*
	(make-hash-table :test 'equal))

(defun add-predicate (pred type fluent_or_fixed)
	(setf (gethash pred *spectra3*) type)
	(setf (gethash type *spectra1*) (cons pred (gethash type *spectra1*)))
	(setf (gethash type *spectra2*) fluent_or_fixed))

;; add some initial knowledge
(add-predicate 'is_happy 'emotion 'fluent)
(add-predicate 'is_sad 'emotion 'fluent)
(add-predicate 'is_angry 'emotion 'fluent)
(add-predicate 'is_bored 'emotion 'fluent)
(add-predicate 'is_very_afraid 'emotion 'fluent)
(add-predicate 'is_afraid 'emotion 'fluent)
(add-predicate 'is_extremely_afraid 'emotion 'fluent)
(add-predicate 'is_red 'color 'fixed)
(add-predicate 'is_green 'color 'fixed)
(add-predicate 'is_blue 'color 'fixed)
(add-predicate 'is_yellow 'color 'fixed)
(add-predicate 'is_a_person 'self_fact 'fixed)
(add-predicate 'is_a_computer_program 'self_fact 'fixed)
(add-predicate 'is_an_animal 'self_fact 'fixed)
(add-predicate 'is_a_rose 'self_fact 'fixed)
(add-predicate 'is_a_lily 'self_fact 'fixed)
(add-predicate 'is_a_flower 'self_fact 'fixed)
(add-predicate 'is_a_tree 'self_fact 'fixed)
(add-predicate 'is_a_plant 'self_fact 'fixed)
(add-predicate 'has_money 'wealth 'fluent)
(add-predicate 'has_tons_of_money 'wealth 'fluent)
(add-predicate 'has_ample_money 'wealth 'fluent)

;; Assignment 3 1.b
(defparameter *subsumed-preds* 
	(make-hash-table :test 'equal))

(defun add-subsum (pred parent)
	(if (null (gethash parent *subsumed-preds*))
		(setf (gethash parent *subsumed-preds*) (list pred))
		(setf (gethash parent *subsumed-preds*) (cons pred (gethash parent *subsumed-preds*)))))
;; Some initial information
(add-subsum 'is_a_rose 'is_a_flower)
(add-subsum 'is_a_lily 'is_a_flower)
(add-subsum 'is_a_flower 'is_a_plant)
(add-subsum 'is_a_tree 'is_a_plant)
(add-subsum 'is_sad 'is_in_a_bad_mood)
(add-subsum 'is_angry 'is_in_a_bad_mood)
(add-subsum 'has_tons_of_money 'has_ample_money)
(add-subsum 'has_ample_money 'has_money)
(add-subsum 'has_100_bucks 'has_50_bucks)
(add-subsum 'has_50_bucks 'has_money)

;;Degrees
(defparameter *initial-degrees*
	(make-hash-table :test 'equal))

(defun add-degree (pred type fluent_or_fixed &optional parent)
	(add-predicate pred type fluent_or_fixed)
	(if (not (null parent))
		(add-subsum pred parent)
		(setf (gethash type *initial-degrees*) pred)))

(add-degree 'is_tired 'tiredness 'fluent)
(add-degree 'is_a_little_tired 'tiredness 'fluent 'is_tired)
(add-degree 'is_very_tired 'tiredness 'fluent 'is_a_little_tired)
(add-degree 'is_extremely_tired 'tiredness 'fluent 'is_very_tired)


;; 2. Recursion and mapping
;; a. 
(defun count-occurrences (atm lst)
	"count how many times the atom occurs in the list"
	(if (not (atom atm))
		(and (format t "This program expects an atom type as first argument")
			(return-from count-occurrences 0)))
	(if (atom lst)
		(if (equal atm lst)
			(return-from count-occurrences 1)
			(return-from count-occurrences 0)))
	(if (equal (car lst) atm)
		(return-from count-occurrences (+ 1 (count-occurrences atm (cdr lst))))
		(return-from count-occurrences 
			(+ (count-occurrences atm (cdr lst)) (count-occurrences atm (car lst))))))
;;Test
; (defun test-count-occurrences ()
; 	"Testing function for count-occurrences"
; 	(and (eql 1 (count-occurrences 'a 'a))
; 		 (eql 0 (count-occurrences 'a 'b))
; 		 (eql 1 (count-occurrences 5 '(5)))
; 		 (eql 3 (count-occurrences 'a '(a (b (c a) a))))))
; (format t "Test for part 2.a: ~%")
; (if (test-count-occurrences)
; 	(format t "part 2.a passed ~%")
; 	(format t "part 2.a did not pass ~%"))

;; b. 
(defun flatten (lst)
	"outputs the atoms that occur in the list structure lst from left to right"
	(if (equal lst '())
		(return-from flatten '()))
	(if (atom lst)
		(return-from flatten (list lst))
		(return-from flatten (append (flatten (car lst)) (flatten (cdr lst))))))
;;Test
; (defun test-flatten ()
; 	"Testing function for flatten"
; 	(and (equal '(A) (flatten 'a))
; 		 (equal '(A) (flatten '(a)))
; 		 (equal '(I SAW THAT THE OLD MAN LOOKED UP AND SMILED RATHER SLYLY) 
; 		 	(flatten '(I (saw (that the) (old man)) (looked up (and smiled) (rather slyly)))))))
; (format t "Test for part 2.b: ~%")
; (if (test-flatten)
; 	(format t "part 2.b passed ~%")
; 	(format t "part 2.b did not pass ~%"))

(defparameter *ep-memory* '())
;; a. 
(defun list-has (w lst)
	"helper funstion to check if w is in the list"
	(if (equal lst '())
		(return-from list-has 0)
		(if (equal w (car lst))
			(return-from list-has 1)
			(list-has w (cdr lst)))))

(defparameter *beliefs* (make-hash-table :test 'equal))
;; b
(defun remove-formula (w kb)
	"remove formula from knowledge base"
	(cond 	((eql 2 (list-length w))
				(let ((k1 w)
						(k2 (list (car w) nil)))
						(if (eql 1 (list-has w (gethash k1 kb)))
							(setf (gethash k1 kb) (remove w (gethash k1 kb) :test 'equal )))
						(if (eql 1 (list-has w (gethash k2 kb)))
							(setf (gethash k2 kb) (remove w (gethash k2 kb) :test 'equal )))
					))
			((eql 3 (list-length w))
				(let ((k1 w)
					  (k2 (list (car w) (nth 1 w) nil))
					  (k3 (list (car w) nil (nth 2 w)))
					  (k4 (list (car w) nil nil)))
					(if (eql 1 (list-has w (gethash k1 kb)))
						(setf (gethash k1 kb) (remove w (gethash k1 kb) :test 'equal )))
					(if (eql 1 (list-has w (gethash k2 kb)))
						(setf (gethash k2 kb) (remove w (gethash k2 kb) :test 'equal )))
					(if (eql 1 (list-has w (gethash k3 kb)))
						(setf (gethash k3 kb) (remove w (gethash k3 kb) :test 'equal )))
					(if (eql 1 (list-has w (gethash k4 kb)))
						(setf (gethash k4 kb) (remove w (gethash k4 kb) :test 'equal )))
						)))
	nil)

;; 3. Maintaining beliefs about the world
;; c. 
;; keeps track of a list of formulas that are currently true

(defun store-formula (w kb)
	"store new knowledge to the global knowledge base beliefs"
	;; remove incompatible formulas
	(if (equal (gethash (gethash (car w) *spectra3*) *spectra2*) 'fluent)
		(let ((type (gethash (car w) *spectra3*)))
			(let ((spectrum (gethash type *spectra1*)))
				(loop for pred in spectrum 
					do (remove-formula (cons pred (cdr w)) kb)))))
	(cond 	((eql 2 (list-length w))
				;; remove incompatible formulas
				; (if (equal (gethash (gethash (car w) *spectra3*) *spectra2*) 'fluent)
				; 	(let ((type (gethash (car w) *spectra3*)))
				; 		(let ((spectrum (gethash type *spectra1*)))
				; 			(loop for pred in spectrum 
				; 				do (remove-formula (list pred (cadr w)) kb)))))
				(let ((k1 w)
					  (k2 (list (car w) nil)))
					(if (eql 0 (list-has w (gethash k1 kb)))
						(setf (gethash k1 kb) (cons w (gethash k1 kb))))
					(if (eql 0 (list-has w (gethash k2 kb)))
						(setf (gethash k2 kb) (cons w (gethash k2 kb))))))
			((eql 3 (list-length w))
				(let ((k1 w)
					  (k2 (list (car w) (nth 1 w) nil))
					  (k3 (list (car w) nil (nth 2 w)))
					  (k4 (list (car w) nil nil)))
					(if (eql 0 (list-has w (gethash k1 kb)))
						(setf (gethash k1 kb) (cons w (gethash k1 kb))))
					(if (eql 0 (list-has w (gethash k2 kb)))
						(setf (gethash k2 kb) (cons w (gethash k2 kb))))
					(if (eql 0 (list-has w (gethash k3 kb)))
						(setf (gethash k3 kb) (cons w (gethash k3 kb))))
					(if (eql 0 (list-has w (gethash k4 kb)))
						(setf (gethash k4 kb) (cons w (gethash k4 kb)))))))
		(setf *ep-memory* (cons (cons w (butlast (multiple-value-list (get-decoded-time)) 3)) *ep-memory*))
		nil)

(defparameter *Wumpus-location*
	'School)
; (if (not (null (car (cadr (cdr (gethash '(is_at WUMPUS nil) *beliefs*))))))
; 	(setf *Wumpus-location* (car (cadr (cdr (gethash '(is_at WUMPUS nil) *beliefs*))))))

(store-formula 
	(list 'RULE (list nil (list 'is_at 'AG *Wumpus-location*)) 
		'((set_state is_very_afraid))) *beliefs*)

(store-formula 
	'(RULE ((exercise AG)) ((++ tiredness))) *beliefs*)


;;Test
; (defun test-store-formula ()
;   "Testing function for store-formula"
;   (store-formula '(likes AG) *beliefs*)
;   (return-from test-store-formula (and 
;   						(equal '((likes AG)) (gethash '(likes AG) *beliefs*))
; 				       	(equal '((likes AG)) (gethash '(likes nil) *beliefs*))
; 				       	(eql 1 (list-has '(likes AG)  (car *ep-memory*))))))

; (format t "Test for part 3.a: ~%")
; (if (test-store-formula)
;        (format t "part 3.a passed ~%")
;        (format t "part 3.a did not pass ~%"))
    

;;Test
; (defun test-remove-formula ()
;   "Testing function for remove-formula"
;   (remove-formula '(likes AG Guru) *beliefs*)
;   (return-from test-remove-formula (and (equal '() (gethash '(likes AG Guru) *beliefs*))
; 					(equal '() (gethash '(likes nil Guru) *beliefs*))
; 					(equal '() (gethash '(likes AG nil) *beliefs*))
; 					(equal '() (gethash '(likes nil nil) *beliefs*))
; 					(equal '() *ep-memory*))))
; (format t "Test for part 3.b: ~%")
; (if (test-remove-formula)
;     (format t "part 3.b passed ~%")
;     (format t "part 3.b did not pass ~%"))

