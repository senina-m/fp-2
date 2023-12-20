(load "~/quicklisp/setup.lisp")
(load "src/trie.lisp")
(ql:quickload :lisp-unit)

(lisp-unit:define-test make-trie
    (;; to do smth
        (lisp-unit:assert-equal "abc" (make-trie ))))

(lisp-unit:run-tests)