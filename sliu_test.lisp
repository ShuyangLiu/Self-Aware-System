;; CSC 291 - Lisp Assignment 2
;; Shuyang Liu
;; March 01, 2017

(load "sliu_src.lisp")

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun cleanup ()
	(setf *ep-memory* '())
	(setf *beliefs* (make-hash-table :test 'equal)))

(defparameter *watch_movie*
	(make-op
		:action '(watch_movie AG)
		:preconds '((has_money AG) (is_at AG Plaza) (is_empty-handed AG))
		:effects '((is_happy AG) (not (has_money AG)))))

(defparameter *go_to_plaza*
	(make-op
		:action '(goes_to AG Plaza)
		:preconds '((has_money AG) (not (is_at AG Plaza)) (is_empty-handed AG))
		:effects '((is_at AG Plaza))))

;; 1.a.
(defun test_look-up-kb ()
	(store-formula '(has_money AG) *beliefs*)
	(and 
		(eql t (look-up-kb '(has_money AG) *beliefs*))
		(eql nil (look-up-kb '(is_at AG Plaza) *beliefs*))))
(if (test_look-up-kb)
	(format t "1.a.1 look-up-kb test passed~%")
	(format t "1.a.1 look-up-kb test not passed~%"))
(cleanup)

(defun test_check-preconds ()
	(store-formula '(has_money AG) *beliefs*)
	(store-formula '(is_at AG Plaza) *beliefs*)
	(store-formula '(is_empty-handed AG) *beliefs*)
	(let ((pred '((has_money AG) (is_at AG Plaza) (is_empty-handed AG)))
		(predf '((has_money AG) (is_at AG Plaza) (is_empty-handed AG) (is_happy AG))))
		(and (check-preconds pred *beliefs*)
			(not (check-preconds predf *beliefs*)))))
(if (test_check-preconds)
	(format t "1.a.2 check-preconds test passed~%")
	(format t "1.a.2 check-preconds test not passed~%"))
(cleanup)

(defun test_consider-action ()
		(store-formula '(has_money AG) *beliefs*)
		(store-formula '(is_at AG Plaza) *beliefs*)
		(store-formula '(is_empty-handed AG) *beliefs*)
		(let 	((res1 (equal 
						(op-effects *go_to_movie*) 
						(consider-action *go_to_movie*)))
				(res2 (and 
						(null (remove-formula '(has_money AG) *beliefs*))
						(null (consider-action *go_to_movie*)))))
			(and res1 res2)))
(if (test_consider-action)
	(format t "1.a.3 consider-action test passed~%")
	(format t "1.a.3 consider-action test not passed~%"))
(cleanup)

(defun test_feasible-actions ()
		(store-formula '(has_money AG) *beliefs*)
		(store-formula '(is_at AG Plaza) *beliefs*)
		(store-formula '(is_empty-handed AG) *beliefs*)
		(let ((res1 (equal 
						(list (list *go_to_movie* (op-effects *go_to_movie*)))
						(feasible-actions (list *go_to_movie* *go_to_plaza*))))
				(res2 (and
						(null (remove-formula '(is_at AG Plaza) *beliefs*))
						(equal
							(list (list *go_to_plaza* (op-effects *go_to_plaza*)))
							(feasible-actions (list *go_to_movie* *go_to_plaza*))))))
			(and res1 res2)))
(if (test_feasible-actions)
	(format t "1.b feasible-actions test passed~%")
	(format t "1.b feasible-actions test not passed~%"))
(cleanup)
 