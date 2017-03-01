;; CSC 291 - Lisp Assignment 2
;; Shuyang Liu
;; March 01, 2017

;; The Test file for sliu_src.lisp
;; Each test- function corresponds to a function defined in sliu_src.lisp
;; To Run the tests, simply load this test file in the REPL using (load sliu_test.lisp)

(load "sliu_src.lisp")

;; Helper Function to get a list of keys in the hashtable
(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

;; restore the configuration of *beliefs* and *ep-memory* after testing 
(defun cleanup ()
	(setf *ep-memory* '())
	(setf *beliefs* (make-hash-table :test 'equal))
	(setf *falsehoods-by-default* (make-hash-table :test 'equal))
	(setf (gethash '(is_a_person AG) *falsehoods-by-default*) t)
	(setf (gethash '(hates AG nil) *falsehoods-by-default*) t)
	(store-formula '(likes AG USER) *beliefs*))

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
		:effects '((is_at AG Plaza))))

;; Testing the helper functions 
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
;;1.a testing the consider-action function, which should return nil if the preconditions are not true, otherwise a list of effects 
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

;; 1.b testing for feasible-actions, should return a list of feasible actions with their effects 
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

;; Testing for do-action, compare the changes in *beliefs* to see if do-action works correctly
(defun test_do-action ()
	(let ((res1 (do-action *go_to_movie*)))
		(store-formula '(has_money AG) *beliefs*)
		(store-formula '(is_at AG Plaza) *beliefs*)
		(store-formula '(is_empty-handed AG) *beliefs*)
		(do-action *go_to_movie*)
		(let ((res2 (and (null (gethash '(has_money AG) *beliefs*))
							(not (null (gethash '(is_happy AG) *beliefs*)))
							(not (null (gethash '(goes_to_movie AG) *beliefs*))))))
			(and (null res1) res2))))
 
(if (test_do-action)
	(format t "1.c do-action test passed~%")
	(format t "1.c do-action test not passed~%"))
(cleanup)

;; Testing for generat-English, comparing the string outputs
(defun test_generate-English ()
	(string-equal
		(concatenate 'string "I" " " "HAS MONEY" (string #\linefeed) "YOU" " " "IS AT" " " "PLAZA" (string #\linefeed))
		(generate-English '((has_money AG) (is_at USER Plaza)))))
(if (test_generate-English)
	(format t "2.a generate-English test passed~%")
	(format t "2.a generate-English test not passed~%"))
(cleanup)

;;Testing for the query function '?'
(defun test-query ()
	(and
		(string-equal (? '(likes AG USER)) (concatenate 'string (string #\Tab) "YES, I LIKES YOU" (string #\linefeed)))
		(string-equal (? '(likes Guru USER)) (concatenate 'string (string #\Tab) "I DO NOT HAVE AN ANSWER FOR THAT" (string #\linefeed)))
		(string-equal (? '(hates AG USER)) (concatenate 'string (string #\Tab) "NO, IT IS NOT THE CASE THAT I HATES YOU" (string #\linefeed)))))

(if (test-query)
	(format t "2.b ? test passed~%")
	(format t "2.b ? test not passed~%"))
(cleanup)
 
