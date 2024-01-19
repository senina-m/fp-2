(load "~/quicklisp/setup.lisp")
(load "src/trie.lisp")
(ql:quickload :lisp-unit)
(ql:quickload :check-it)

;  (with-open-file (output "tmp1"
;                             :direction :output
;                             :if-exists :supersede)
;                     (progn
;                     (mapcar #'(lambda(x) (format output "~a~%" x)) (trie:words t1))
;                     (trie:print-trie output t1)))
;     (with-open-file (output "tmp2"
;                             :direction :output
;                             :if-exists :supersede)
;                     (progn 
;                     (mapcar #'(lambda(x) (format output "~a~%" x)) (sort (trie:words t2) #'string-lessp))
;                     (trie:print-trie output t2)))

(defun compare-tries (t1 t2)
    ;это будет работать пока мы уверены, что words возвращает set т.е. без повторений
    (not (set-exclusive-or (trie:words t1) (trie:words t2) :test #'string=)))

(defun gen-str (len)
    (check-it:generate (check-it:generator (string :min-length 2 :max-length len))))

(defun gen-str-list (str-len list-len)
    (loop for i from 1 to list-len
      collect (gen-str str-len)))

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
                (trie:words (trie:make-trie :initial-contents initial-contents))))
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
    (let ((lst1 (gen-str-list 4 5)))
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
    (let ((lst1 (gen-str-list 4 5))
          (lst2 (gen-str-list 4 5)))
            (compare-tries (trie:sum-tries 
                                (trie:make-trie :initial-contents lst1)
                                (trie:make-trie :initial-contents lst2))
                           (trie:sum-tries 
                                (trie:make-trie :initial-contents lst2)
                                (trie:make-trie :initial-contents lst1)))))

(defun insert-property-based ()
    (let ((lst (gen-str-list 4 5)))
        (let ((isrt (car lst))
              (init (cdr lst)))
            (compare-tries (trie:insert (trie:make-trie :initial-contents init) isrt)
                                (trie:make-trie :initial-contents lst)))))

(lisp-unit:define-test test-1
    (lisp-unit:assert-true (creation-test "test/test1" "test/test1-answers" '())))

(lisp-unit:define-test test-2
    (lisp-unit:assert-true (creation-test "test/test2" "test/test2-answers" '("b" "a" "c"))))

(lisp-unit:define-test test-3
    (lisp-unit:assert-true (creation-test "test/test3" "test/test3-answers" '("bac" "acd" "acdb"))))

(lisp-unit:define-test test-4
    (lisp-unit:assert-true (insert-test "test/test4" "test/test4-answers" '() "name")))

(lisp-unit:define-test test-5
    (lisp-unit:assert-true (insert-test "test/test5" "test/test5-answers" '("b" "a" "c") "c")))

(lisp-unit:define-test test-6
    (lisp-unit:assert-true (insert-test "test/test6" "test/test6-answers" '("b" "a" "c") "ca")))

(lisp-unit:define-test test-7
    (lisp-unit:assert-true (words-test "test/test7" "test/test7-answers" '())))

(lisp-unit:define-test test-8
    (lisp-unit:assert-true (words-test "test/test8" "test/test8-answers" '("b" "a" "c"))))

(lisp-unit:define-test test-9
    (lisp-unit:assert-true (words-test "test/test9" "test/test9-answers" '("bcd" "bcdf" "cab"))))

(lisp-unit:define-test test-10
    (lisp-unit:assert-true (map-test "test/test10" "test/test10-answers" '() (lambda (x) (length x)))))

(lisp-unit:define-test test-11
    (lisp-unit:assert-true (map-test "test/test11" "test/test11-answers" '("b" "ac" "acb") (lambda (x) (length x)))))

(lisp-unit:define-test test-12
    (lisp-unit:assert-true (search-test "test/test12" "test/test12-answers" '("bcd" "bcdf" "cab") "")))

(lisp-unit:define-test test-13
    (lisp-unit:assert-true (search-test "test/test13" "test/test13-answers" '("bcd" "bcdf" "cab") "bc")))

(lisp-unit:define-test test-14
    (lisp-unit:assert-true (search-test "test/test14" "test/test14-answers" '("bcd" "bcdf" "cab") "d")))

(lisp-unit:define-test test-15
    (lisp-unit:assert-true (delete-test "test/test15" "test/test15-answers" '("bcd" "bcdf" "cab") "")))

(lisp-unit:define-test test-16
    (lisp-unit:assert-true (delete-test "test/test16" "test/test16-answers" '("bcd" "bcdf" "cab") "b")))

(lisp-unit:define-test test-17
    (lisp-unit:assert-true (delete-test "test/test17" "test/test17-answers" '("bcd" "bcdf" "cab") "ds")))

(lisp-unit:define-test test-18
    (lisp-unit:assert-true (sum-test "test/test18" "test/test18-answers" '("bcd" "bcdf" "cab") '("bcd" "bc" "acb"))))

(lisp-unit:define-test test-19
    (lisp-unit:assert-true (sum-test "test/test19" "test/test19-answers" '("bcd" "bcdf" "cab") "ds")))

(lisp-unit:define-test test-20
    (lisp-unit:assert-true (create-property-based)))

(lisp-unit:define-test test-21
    (lisp-unit:assert-true (sum-property-based)))

(lisp-unit:define-test test-22
    (lisp-unit:assert-true (insert-property-based)))

(lisp-unit:run-tests)