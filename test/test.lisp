(load "~/quicklisp/setup.lisp")
(load "src/trie.lisp")
(ql:quickload :lisp-unit)

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

; (lisp-unit:define-test test-12
;     (lisp-unit:assert-true (map-test "test/test12" "test/test12-answers" '("bcd" "bcdf" "cab"))))

(load "src/trie.lisp")

(lisp-unit:run-tests)