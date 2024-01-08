(load "~/quicklisp/setup.lisp")
(load "src/trie.lisp")
(ql:quickload :lisp-unit)

(defun read-words (file-name)
    (with-open-file (stream file-name)
      (loop while (peek-char nil stream nil nil)
           collect (read-sequence (read stream)))))

(defun test1()
    (let ((words (read-words "words.txt")))
        (with-open-file (stream "result.txt" :direction :output
                                             :if-does-not-exist :create)
            (print-trie stream (make-trie :initial-contents words)))))


(lisp-unit:run-tests)