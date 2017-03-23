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
		:effects '((is_at AG Plaza) (not (car (gethash '(is_at AG nil) *beliefs*))))))

(defparameter *go_to_school*
	(make-op
		:action '(goes_to AG School)
		:preconds '((has_money AG) (not (is_at AG School)))
		:effects '((is_at AG School) (not (car (gethash '(is_at AG nil) *beliefs*))))))



