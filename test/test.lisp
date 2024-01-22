(load "~/quicklisp/setup.lisp")
(load "src/trie.lisp")
(ql:quickload :lisp-unit)
(ql:quickload :check-it)

					;  (with-open-file (output "tmp1"
					;                             :direction :output
					;                             :if-exists :supersede)
					;                     (progn
					;                     (mapcar #'(lambda(x) (format output "~a~%" x)) (trie:list-words t1))
					;                     (trie:print-trie output t1)))
					;     (with-open-file (output "tmp2"
					;                             :direction :output
					;                             :if-exists :supersede)
					;                     (progn
					;                     (mapcar #'(lambda(x) (format output "~a~%" x)) (sort (trie:list-words t2) #'string-lessp))
					;                     (trie:print-trie output t2)))

(defun compare-tries (t1 t2)
					;это будет работать пока мы уверены, что words возвращает set т.е. без повторений
  (not (set-exclusive-or (trie:list-words t1) (trie:list-words t2) :test #'string=)))

(defun gen-str (len)
  (let ((l
	 (check-it:generate (check-it:generator (list (character #\a #\z) :min-length 0 :max-length len)))))
    (if (null l)
	nil
      (concatenate 'string l))))

(defun gen-str-list()
  (let ((list-len (check-it:generate (check-it:generator (integer -1 10))))
        (str-len (check-it:generate (check-it:generator (integer 0 10)))))
    (loop for i from 0 to list-len
	  collect (gen-str str-len))))

(defun compare-files (fst-name snd-name)
  (with-open-file (i-res fst-name
                         :direction :input
                         :if-does-not-exist :error)
		  (with-open-file (i-data snd-name
					  :direction :input
					  :if-does-not-exist :error)
				  (let ((result t))
				    (loop :for res = (read-line i-res nil :eof)
					  :for data = (read-line i-data nil :eof)
					  :do (if (string= res data) nil (setf result nil))
					  :until (and (eq res :eof) (eq data :eof)))
					; (format t "RES=~a~%" result)
				    (return-from compare-files result)))))

(defun creation-test(res-name answer-name initial-contents)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		  (trie:print-trie output (trie:make-trie :initial-contents initial-contents)))
  (compare-files res-name answer-name))

(defun insert-test(res-name answer-name initial-contents insert-contents)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
					; (format stream "(#\\~c" prefix))
		  (trie:print-trie output (trie:insert (trie:make-trie :initial-contents initial-contents) insert-contents)))
  (compare-files res-name answer-name))

(defun words-test(res-name answer-name initial-contents)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		  (mapcar #'(lambda(x) (format output "~a~%" x))
			  (trie:list-words (trie:make-trie :initial-contents initial-contents))))
  (compare-files res-name answer-name))

(defun map-test(res-name answer-name initial-contents func)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		  (mapcar #'(lambda(x) (format output "~a~%" x))
			  (trie:map-trie (trie:make-trie :initial-contents initial-contents) func)))
  (compare-files res-name answer-name))

(defun search-test(res-name answer-name initial-contents word-prefix)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		  (mapcar #'(lambda(x) (format output "~a~%" x))
			  (trie:search-trie (trie:make-trie :initial-contents initial-contents) word-prefix)))
  (compare-files res-name answer-name))

(defun delete-test(res-name answer-name initial-contents delete-prefix)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		  (trie:print-trie output (trie:delete-trie (trie:make-trie :initial-contents initial-contents) delete-prefix)))
  (compare-files res-name answer-name))

(defun sum-test(res-name answer-name init-1 init-2)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		  (trie:print-trie output (trie:sum-tries (trie:make-trie :initial-contents init-1) (trie:make-trie :initial-contents init-2))))
  (compare-files res-name answer-name))

(defun create-property-based()
  (let ((lst1 (gen-str-list)))
    (let ((lst2  (reverse (copy-list lst1))))
      (compare-tries (trie:make-trie :initial-contents lst1)
                     (trie:make-trie :initial-contents lst2)))))

					; (with-open-file (output "tmp"
					;                         :direction :output
					;                         :if-exists :supersede)
					;                 (progn
					;                 (mapcar #'(lambda(x) (format output "~a~%" x)) (sort lst1  #'string-lessp))
					;                 (mapcar #'(lambda(x) (format output "~a~%" x)) (sort lst2  #'string-lessp))))

(defun sum-property-based ()
  (let ((lst1 (gen-str-list))
        (lst2 (gen-str-list)))
    (compare-tries (trie:sum-tries
                    (trie:make-trie :initial-contents lst1)
                    (trie:make-trie :initial-contents lst2))
                   (trie:sum-tries
                    (trie:make-trie :initial-contents lst2)
                    (trie:make-trie :initial-contents lst1)))))

(defun insert-property-based ()
  (let ((lst (gen-str-list)))
    (let ((isrt (car lst))
          (init (cdr lst)))
      (compare-tries (trie:insert (trie:make-trie :initial-contents init) isrt)
                     (trie:make-trie :initial-contents lst)))))

(lisp-unit:define-test test-1
		       (lisp-unit:assert-true (creation-test "test/files/test1" "test/files/answers-1" '())))

(lisp-unit:define-test test-2
		       (lisp-unit:assert-true (creation-test "test/files/test2" "test/files/answers-2" '("b" "a" "c"))))

(lisp-unit:define-test test-3
		       (lisp-unit:assert-true (creation-test "test/files/test3" "test/files/answers-3" '("bac" "acd" "acdb"))))

(lisp-unit:define-test test-4
		       (lisp-unit:assert-true (insert-test "test/files/test4" "test/files/answers-4" '() "name")))

(lisp-unit:define-test test-5
		       (lisp-unit:assert-true (insert-test "test/files/test5" "test/files/answers-5" '("b" "a" "c") "c")))

(lisp-unit:define-test test-6
		       (lisp-unit:assert-true (insert-test "test/files/test6" "test/files/answers-6" '("b" "a" "c") "ca")))

(lisp-unit:define-test test-7
		       (lisp-unit:assert-true (words-test "test/files/test7" "test/files/answers-7" '())))

(lisp-unit:define-test test-8
		       (lisp-unit:assert-true (words-test "test/files/test8" "test/files/answers-8" '("b" "a" "c"))))

(lisp-unit:define-test test-9
		       (lisp-unit:assert-true (words-test "test/files/test9" "test/files/answers-9" '("bcd" "bcdf" "cab"))))

(lisp-unit:define-test test-10
		       (lisp-unit:assert-true (map-test "test/files/test10" "test/files/answers-10" '() (lambda (x) (length x)))))

(lisp-unit:define-test test-11
		       (lisp-unit:assert-true (map-test "test/files/test11" "test/files/answers-11" '("b" "ac" "acb") (lambda (x) (length x)))))

(lisp-unit:define-test test-12
		       (lisp-unit:assert-true (search-test "test/files/test12" "test/files/answers-12" '("bcd" "bcdf" "cab") "")))

(lisp-unit:define-test test-13
		       (lisp-unit:assert-true (search-test "test/files/test13" "test/files/answers-13" '("bcd" "bcdf" "cab") "bc")))

(lisp-unit:define-test test-14
		       (lisp-unit:assert-true (search-test "test/files/test14" "test/files/answers-14" '("bcd" "bcdf" "cab") "d")))

(lisp-unit:define-test test-15
		       (lisp-unit:assert-true (delete-test "test/files/test15" "test/files/answers-15" '("bcd" "bcdf" "cab") "")))

(lisp-unit:define-test test-16
		       (lisp-unit:assert-true (delete-test "test/files/test16" "test/files/answers-16" '("bcd" "bcdf" "cab") "b")))

(lisp-unit:define-test test-17
		       (lisp-unit:assert-true (delete-test "test/files/test17" "test/files/answers-17" '("bcd" "bcdf" "cab") "ds")))

(lisp-unit:define-test test-18
		       (lisp-unit:assert-true (sum-test "test/files/test18" "test/files/answers-18" '("bcd" "bcdf" "cab") '("bcd" "bc" "acb"))))

(lisp-unit:define-test test-19
		       (lisp-unit:assert-true (sum-test "test/files/test19" "test/files/answers-19" '("bcd" "bcdf" "cab") "ds")))

(lisp-unit:define-test test-20
		       (lisp-unit:assert-true (create-property-based)))

(lisp-unit:define-test test-21
		       (lisp-unit:assert-true (sum-property-based)))

(lisp-unit:define-test test-22
		       (lisp-unit:assert-true (insert-property-based)))

(lisp-unit:run-tests)

					; (with-open-file (output "tmp"
					; 				:direction :output
					; 				:if-exists :supersede)
					; 				(print-trie output (copy (make-trie :initial-contents '("abf" "ad" "abd")))))

					; (setf ctrie (copy (make-trie :initial-contents '("abf" "ad" "abd"))))

					; (let ((tr (make-trie :initial-contents '("abf" "ad"))))
					;  	(delete-trie tr "ab")
					;  	; (insert tr "abc")
					; 	tr)

					; (let ((trie (make-instance 'trie)))
					; 	(insert (insert trie "a") "bcd"))
