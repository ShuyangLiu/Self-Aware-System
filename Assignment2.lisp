;; CSC 291 - Lisp Assignment 2
;; Shuyang Liu
;; March 01, 2017
(load "Assignment1.lisp")

;; This is the source file that contains all the functions in assignment 2 
;; It has some changes since assignment 2 in order to complete assignment 3
;; It uses some previous function from assignment 1

(store-formula '(is_a_computer_program AG) *beliefs*)

;; Define a struct op with 3 components 
(defstruct op
	action
	preconds
	effects)

;; Define a global op action for testing
(defparameter *go_to_movie*
	(make-op
		:action '(goes_to_movie AG)
		:preconds '((has_money AG) (is_at AG Plaza) (is_empty-handed AG))
		:effects '((is_happy AG) (not (has_money AG)))))

;;1.a
;;helper function, check if a predicate is true in current kb
;; modified for integrating the subsumed-preds and spectra
(defun look-up-kb (key kb)
	(if (null (gethash (car key) *subsumed-preds*))
		(if (equal 'not (car key))
			(null (gethash (cadr key) kb)) ;;negative 
			(if (equal (type-of kb) 'hash-table)
				(not (null (gethash key kb)))
				(and 
					(format t "Second argument of look-up-kb has to be of type HASH-TABLE!~%")
					nil)))
		(if (equal 'not (car key))
			(and (null (gethash (cadr key) kb))
				(reduce (lambda (x y) (and x y))  
					(map 'list #'(lambda (k) (look-up-kb k kb)) 
						(map 'list #'(lambda (x) (cons 'not (list (cons x (cdr (cdr key)))))) 
							(gethash (car (cadr key)) *subsumed-preds*)))))
			(or (not (null (gethash key kb)))
				(reduce (lambda (x y) (or x y))
					(map 'list #'(lambda (k) (look-up-kb k kb))
						(map 'list #'(lambda (x) (cons x (cdr key)))
							(gethash (car key) *subsumed-preds*))))))))

;; helper function for traversing the list
(defun check-preconds (preconds kb)
	(if (null preconds) t
		(and 	(look-up-kb (car preconds) kb) 
				(check-preconds (cdr preconds) kb))))

;; consider-action inspect each of the preconditions of the action and 
;;return the effects of the action if all of the preconditions are true
(defun consider-action (action)
	"check if the preconds are currently true in the kb"
	(let ((pred (op-preconds action)))
		(if (not (check-preconds pred *beliefs*)) nil
			(op-effects action))))

;;1.b
;;feasible-actions gives a list of feasible actions each with their effects
(defun feasible-actions (actions)
	"find all feasible actions, actions has to be a list of op"
	(if (null actions)
		'()
		(if (and (op-p (car actions)) (not (null (consider-action (car actions)))))
			(append (feasible-actions(cdr actions)) 
				(list (list (car actions) (consider-action (car actions)))))
			(feasible-actions (cdr actions)))))

;;1.c
;;helper function for setting ep-memory and removing the formula
(defun helper (effect kb)
	(setf *ep-memory* (cons (cons effect
		(butlast (multiple-value-list (get-decoded-time)) 3)) *ep-memory*))
	(remove-formula (cadr effect) kb))

;; store the effects to *beliefs*
(defun store-effects (effects kb)
	(if (null effects) nil
			(if (equal 'not (car (car effects))) ; negative
				(or 
					(helper (car effects) kb)
					(store-effects (cdr effects) kb))
				(or 
					(store-formula (car effects) kb)
					(store-effects (cdr effects) kb)))))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun get-current-degree-helper (preds subj)
	(if (null preds)
		nil
		(if (not (null (gethash (cons (car preds) subj) *beliefs*)))
			(gethash (cons (car preds) subj) *beliefs*)
			(get-current-degree-helper (cdr preds) subj))))

(defun get-current-degree (type subj)
	(get-current-degree-helper (gethash type *spectra1*) subj))

(defun get-parent (p)
	(loop for pred in (hash-keys *subsumed-preds*)
		do (loop for pr in (gethash pred *subsumed-preds*)
				do (if (equal pr p)
					(return-from get-parent pred))))
	nil)

;; increase or decrease the degree of something
(defun apply-effect (effect subj)
	(if (not (equal (type-of subj) 'list))
		(setf subj (list subj)))
	
	(cond ((equal '++ (car effect))
				(if (not (reduce (lambda (x y) (or x y)) 
						(map 'list (lambda (k) (not (null (gethash (cons k subj) *beliefs*)))) 
							(gethash (cadr effect) *spectra1*))))
					(store-formula (cons (gethash (cadr effect) *initial-degrees*) subj) *beliefs*)
					(let ((current (get-current-degree (cadr effect) subj)))
						(if (not (null (gethash (car current) *subsumed-preds*)))
							(store-formula (cons (car (gethash (car current) *subsumed-preds*)) subj) *beliefs*)))))
		((equal '-- (car effect))
			(if (reduce (lambda (x y) (or x y)) 
						(map 'list (lambda (k) (not (null (gethash (cons k subj) *beliefs*)))) 
							(gethash (cadr effect) *spectra1*)))
				(if (null (get-parent (car (get-current-degree (cadr effect) subj))))
					(remove-formula (get-current-degree (cadr effect) subj) *beliefs*)
					(let ((parent (get-parent (car (get-current-degree (cadr effect) subj)))))
						(store-formula (cons parent subj) *beliefs*)))))
		((equal 'set_state (car effect))
			(store-formula (cons (cadr effect) subj) *beliefs*))))

;; Side Effects
(defun do-side-effects (action)
	(let ((rules (gethash '(RULE nil nil) *beliefs*)))
		(loop for rule in rules
			do (let ((conds (cadr rule))
						(side_effects (cadr (cdr rule))))
				(if (and (or (equal (car conds) action) (null (car conds))) (check-preconds (cdr conds) *beliefs*))
					(loop for effect in side_effects
						do (apply-effect effect (cdr action))))))))


;; performing the action, store the action and efects into kb
(defun do-action (action)
	(if (not (null (consider-action action)))
		(or (store-effects (op-effects action) *beliefs*)
			(store-formula (op-action action) *beliefs*)
			(do-side-effects (cons (car (op-action action)) (cadr (op-action action)))))
		(format t "the given action is not feasible currently!~%")))

;;2.a
;; generating corresponding English sentences
(defun generate-English (formulas)
	(if (null formulas)
		(string "")
		(cond 
			((eql 2 (list-length (car formulas)))
				(let ((e1 (string (cadr (car formulas))))
						(e2 (substitute #\Space #\_ (string (car (car formulas))))))
					(if (equal e1 "AG")
						(setf e1 "I"))
					(if (equal e1 "USER")
						(setf e1 "YOU"))
					(concatenate 'string e1 " " e2 (string #\linefeed) (generate-English (cdr formulas)))))
			((eql 3 (list-length (car formulas)))
				(let ((e1 (string (cadr (car formulas))))
						(e2 (substitute #\Space #\_  (string (car (car formulas)))))
						(e3 (string (cadr (cdr (car formulas))))))
					(if (equal e1 "AG")
						(setf e1 "I"))
					(if (equal e1 "USER")
						(setf e1 "YOU"))
					(if (equal e3 "AG")
						(setf e3 "I"))
					(if (equal e3 "USER")
						(setf e3 "YOU"))
					(concatenate 'string e1 " " e2 " " e3 (string #\linefeed) 
						(generate-English (cdr formulas))))))))

;;2.b
;; global hash table that stores the predicats which are default to false
(defparameter *falsehoods-by-default* (make-hash-table :test 'equal))
;;Some predefined truth
;;(setf (gethash '(is_a_person AG) *falsehoods-by-default*) t)
;;(setf (gethash '(hates AG nil) *falsehoods-by-default*) t)
(store-formula '(likes AG USER) *beliefs*)
;; whether the query question is a wh-question
(defun is-wh (question)
	(if (null question) nil
		(if (equal #\? (char (string (car question)) 0)) t
			(is-wh (cdr question)))))
;; transform the question into the form that can be used for querying in *beliefs*
(defun transfer-query (question)
	(if (null question) 
		'()
		(if (equal #\? (char (string (car question)) 0))
			(append (list nil) (transfer-query (cdr question)))
			(append (list (car question)) (transfer-query (cdr question))))))
;; Give an answer to the wh-question
(defun ans-wh (query str)
		(setf str (concatenate 'string str (generate-English (gethash query *beliefs*))))
		(setf str (concatenate 'string str (string #\linefeed)))
		(if (not (null (gethash (car query) *subsumed-preds*)))
			(loop for pred in (gethash (car query) *subsumed-preds*)
				do (setf str (string-trim (string #\linefeed) (ans-wh (cons pred (cdr query)) str)))))
		(return-from ans-wh (string-trim (string #\linefeed) str)))

;; check to see if there is any match in *falsehoods-by-defalts*
(defun no-match-pattern-query (query)
	(cond
		((eql 2 (list-length query))
			(and 
				(null (gethash query *falsehoods-by-default*))
				(null (gethash (list (car query) nil) *falsehoods-by-default*))))
		((eql 3 (list-length query))
			(and 
				(null (gethash query *falsehoods-by-default*))
				(null (gethash (list (car query) nil (cadr (cdr query))) *falsehoods-by-default*))
				(null (gethash (list (car query) (cadr query) nil) *falsehoods-by-default*))
				(null (gethash (list (car query) nil nil) *falsehoods-by-default*))))))

;; iterate each element of the lst to see if any of them is currently in the beliefs
(defun in-beliefs (lst obj)
	(if (null lst) nil
		(if (not (null (gethash (list (car lst) obj) *beliefs*)))
			(gethash (list (car lst) obj) *beliefs*)
			(in-beliefs (cdr lst) obj))))

;; Answer the yes/no-question
(defun ans-yn (query)
	(if (null (gethash query *beliefs*))
		(if (null (gethash (car query) *subsumed-preds*))
			(if (no-match-pattern-query query) 
				(if (null (in-beliefs (gethash (gethash (car query) *spectra3*) *spectra1*) (cadr query)))
					(if (not (equal (cadr query) 'AG))
						(concatenate 'string (string #\Tab) "I DO NOT HAVE AN ANSWER FOR THAT" (string #\linefeed))
						(concatenate 'string (string #\Tab) "NO, IT IS NOT CASE THAT " 
							(generate-English (list query))))
					(concatenate 'string (string #\Tab) "NO, " 
						(generate-English 
							(in-beliefs (gethash (gethash (car query) *spectra3*) *spectra1*) (cadr query)))))
				(concatenate 'string (string #\Tab) "NO, IT IS NOT THE CASE THAT " 
					(generate-English (list query))))
			(or
				(loop for pred in (gethash (car query) *subsumed-preds*)
					do (if (equal "YES" (subseq (string-trim (string #\Tab) (ans-yn (cons pred (cdr query)))) 0 3))
						(return-from ans-yn 
							(ans-yn (cons pred (cdr query))))))
				(return-from ans-yn 
					(concatenate 'string (string #\Tab) "NO, IT IS NOT THE CASE THAT " 
						(generate-English (list query))))))
		(concatenate 'string (string #\Tab) "YES, " (generate-English (list query)))))


;; main function for answer questions
(defun ? (question)
	(let ((query (transfer-query question)))
		(if (is-wh question)
			(ans-wh query "")
			(ans-yn query))))
