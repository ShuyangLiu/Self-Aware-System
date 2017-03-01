;; CSC 291 - Lisp Assignment 2
;; Shuyang Liu
;; March 01, 2017
(load "Assignment1.lisp")

;; This is the source file that contains all the functions in assignment 2
;; It uses some previous function from assignment 1

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
(defun look-up-kb (key kb)
	(if (equal 'not (car key))
		(null (gethash (cadr key) kb)) ;;negative 
		(if (equal (type-of kb) 'hash-table)
			(not (null (gethash key kb)))
			(and 
				(format t "Second argument of look-up-kb has to be of type HASH-TABLE!~%")
				nil))))

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
			(append (feasible-actions(cdr actions)) (list (list (car actions) (consider-action (car actions)))))
			(feasible-actions (cdr actions)))))

;;1.c
;;helper function for setting ep-memory and removing the formula
(defun helper (effects kb)
	(setf *ep-memory* (cons (cons (car effects) (butlast (multiple-value-list (get-decoded-time)) 3)) *ep-memory*))
	(remove-formula (cadr (car effects)) kb))

;; store the effects to *beliefs*
(defun store-effects (effects kb)
	(if (null effects) nil
		(or
			(if (equal 'not (car (car effects))) ; negative
			(or 
				(helper effects kb)
				(store-effects (cdr effects) kb))
			(or 
				(store-formula (car effects) kb)
				(store-effects (cdr effects) kb))))))
;; performing the action, store the action and efects into kb
(defun do-action (action)
	(if (not (null (consider-action action)))
		(or (store-effects (op-effects action) *beliefs*)
			(store-formula (op-action action) *beliefs*))
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
					(concatenate 'string e1 " " e2 " " e3 (string #\linefeed) (generate-English (cdr formulas))))))))

;;2.b
;; global hash table that stores the predicats which are default to false
(defparameter *falsehoods-by-default* (make-hash-table :test 'equal))
;;Some predefined truth
(setf (gethash '(is_a_person AG) *falsehoods-by-default*) t)
(setf (gethash '(hates AG nil) *falsehoods-by-default*) t)
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
(defun ans-wh (query)
	(generate-English (gethash query *beliefs*)))
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
;; Answer the yes/no-question
(defun ans-yn (query)
	(if (null (gethash query *beliefs*))
		(if (and (no-match-pattern-query query) (not (equal (cadr query) 'AG)))
			(concatenate 'string (string #\Tab) "I DO NOT HAVE AN ANSWER FOR THAT" (string #\linefeed))
			(concatenate 'string (string #\Tab) "NO, IT IS NOT THE CASE THAT " (generate-English (list query))))
		(concatenate 'string (string #\Tab) "YES, " (generate-English (list query)))))
;; main function for answer questions
(defun ? (question)
	(let ((query (transfer-query question)))
		(if (is-wh question)
			(ans-wh query)
			(ans-yn query))))
