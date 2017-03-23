;; CSC 291 - Lisp Assignment 3
;; Shuyang Liu
;; March 22, 2017
(load "Assignment2.lisp")
;;Most of the code are in the "Assignment2.lisp" file as most of them changed the previous implementations

;; Some extra global op actions for testing
(defparameter *watch_movie*
	(make-op
		:action '(watch_movie AG)
		:preconds '((has_money AG) (is_at AG Plaza) (is_empty-handed AG))
		:effects '((is_happy AG) (not (has_money AG)))))

(defparameter *go_to_plaza*
	(make-op
		:action '(goes_to AG Plaza)
		:preconds '((has_money AG) (not (is_at AG Plaza)) (is_empty-handed AG))
		:effects (list (list 'not (list 'is_at 'AG 'CURRENT)) '(is_at AG Plaza))))

(defparameter *go_to_school*
	(make-op
		:action '(goes_to AG School)
		:preconds '((has_money AG) (not (is_at AG School)))
		:effects (list (list 'not (list 'is_at 'AG 'CURRENT)) '(is_at AG School))))

(defparameter *go_to_home*
	(make-op
		:action '(goes_to AG Home)
		:preconds '((not (is_at AG Home)))
		:effects (list (list 'not (list 'is_at 'AG 'CURRENT)) '(is_at AG Home))))

;; 3. Choosing and Performing an Action
(defun init-priority (table actions)
	(loop for action in actions
		do (setf (gethash action table) 0)))

(defun choose-max-priority (table)
	(reduce (lambda (x y) (if (> (gethash x table) (gethash y table)) x y)) 
			(hash-keys table)))

(defun choose-action (actions)
	(let ((priority_values (make-hash-table :test 'equal)))
		(init-priority priority_values actions)
		(loop for rule in (gethash '(PRIORITY nil nil) *beliefs*)
			do (if (and (look-up-kb (cadr rule) *beliefs*) (not (null (consider-action (car (cadr (cdr rule)))))))
				(setf (gethash (car (cadr (cdr rule))) priority_values) (cadr (cadr (cdr rule))))))
		(choose-max-priority priority_values)))


(defparameter *all-actions*
	(list *watch_movie* *go_to_plaza* *go_to_school* *go_to_home*))
;; 4. main running function
(defun go! ()
	(do-action (choose-action *all-actions*)))

(defun stop! ()
	(quit))


