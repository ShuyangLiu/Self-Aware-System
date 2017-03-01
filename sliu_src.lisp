;; CSC 291 - Lisp Assignment 2
;; Shuyang Liu
;; March 01, 2017
(load "Assignment1.lisp")

(defstruct op
	action
	preconds
	effects)

(defparameter *go_to_movie*
	(make-op
		:action '(goes_to_movie AG)
		:preconds '((has_money AG) (is_at AG Plaza) (is_empty-handed AG))
		:effects '((is_happy AG) (not (has_money AG)))))

;;1.a
(defun look-up-kb (key kb)
	(if (equal 'not (car key))
		(null (gethash (cadr key) kb)) ;;negative 
		(if (equal (type-of kb) 'hash-table)
			(not (null (gethash key kb)))
			(and 
				(format t "Second argument of look-up-kb has to be of type HASH-TABLE!~%")
				nil))))

(defun check-preconds (preconds kb)
	(if (null preconds) t
		(and 	(look-up-kb (car preconds) kb) 
				(check-preconds (cdr preconds) kb))))

(defun consider-action (action)
	"check if the preconds are currently true in the kb"
	(let ((pred (op-preconds action)))
		(if (not (check-preconds pred *beliefs*)) nil
			(op-effects action))))

;;1.b
(defun feasible-actions (actions)
	"find all feasible actions, actions has to be a list of op"
	(if (null actions)
		'()
		(if (and (op-p (car actions)) (not (null (consider-action (car actions)))))
			(append (feasible-actions(cdr actions)) (list (list (car actions) (consider-action (car actions)))))
			(feasible-actions (cdr actions)))))

;;1.c
(defun store-effects (effects kb)
	(if (null effects) nil
		(or
			(if (equal 'not (car (car effects))) ; negative
			(or 
				(setf *ep-memory* (cons (cons (car effects) (butlast (multiple-value-list (get-decoded-time)) 3)) *ep-memory*))
				(remove-formula (cadr (car effects)) kb)
				(store-effects (cdr effects) kb))
			(or 
				(store-formula (car effects) kb)
				(store-effects (cdr effects) kb))))))

(defun do-action (action)
	(store-effects (op-effects action) *beliefs*)
	(store-formula (op-action action) *beliefs*))

