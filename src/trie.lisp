(defpackage :trie
 (:use :cl)
 (:export :make-trie 
    :insert 
    :words 
    :search-trie 
    :prefix 
    :suffixes))

  (in-package :trie)
  
  (defclass trie ()
    ((prefix 
        :initarg :prefix
        :initform nil 
        :accessor prefix) 
    (suffixes 
        :initarg :suffixes 
        :initform nil 
        :accessor suffixes)))

(defun trunk (trie word)
            ;; если слово положительной длины оставляем -- иначе запишем 0
    (let* ((char (and (plusp (length word))
                      (char word 0))) 
            ;; запишем в next 
           (next (and char
                    ;; найдём и запишем в next
                    (find char (suffixes trie) :key #'prefix))))
    (cond ((null char) 
            ;; префикс не нашёлся, возвращаем trie и результат -- nil 
            (values trie nil))
          ((null next) 
              ;; префикс нашёлся, возвращаем trie, и остаток, который не нашли
              (values trie word))
          (t 
          ;; нашёлся такой префикс, ищем рекурсивно дальше для word[1:] 
          (trunk next (subseq word 1)))))) 

(defun make-suffix (str)
    (if (> (length str) 0)
        ;; More str to process
        (make-instance 'trie :prefix (char str 0)
                             :suffixes (list (make-suffix (subseq str 1))))
        ;; дошли до конца -- нулевая нода
        (make-instance 'trie)))

(defun insert (trie word)
    ;; находим максимальный префикс, записываем его в rest 
    (multiple-value-bind (trunk rest) (trunk trie word) 
    (cond 
        ((not (null rest))
        ;; если суфикс не нулевой (такого значения ещё нет) добавляем
        (push (make-suffix rest) (suffixes trunk))))) trie)

;; конструктор
(defun make-trie (&key initial-contents)
    (let ((trie (make-instance 'trie)))
        (loop :for word :in initial-contents 
            :do (insert trie word)) trie))

(defun words (trie &optional stack)
    (let ((prefix (slot-value trie 'prefix))
          (suffixes (slot-value trie 'suffixes)))
        (cond 
            ;; если нода нулевая -- это конец 
            ((and (null prefix) (null suffixes)) 
             (if stack 
                ;; преобразуем в строчку слово, до которого дошли
                (list (coerce (reverse stack) 'string))))
                ;; в цикле ходим по всем суфиксам из этой ноды и рекурсивно ждём от них все слова
            (t (loop :for suffix :in suffixes :append (words suffix (if prefix (cons prefix stack) stack)))))))

(defun search-trie (trie prefix)
    (if (string= prefix "")
        ;; если префикс -- пустая строка, выводим все слова дерева
        (words trie)
        ;; если не нашли: ищем, есть ли такой префикс в нашем дереве
        (multiple-value-bind (trunk rest) (trunk trie prefix)
            ;; если есть полное совпадение, т.е. rest = nil
            (if (null rest)
                (let ((len (1- (length prefix))))
                    (mapcar (lambda (x) (concatenate 'string (subseq prefix 0 len) x))
                        (words trunk)))))))

(defun print-trie (stream trie &optional (indent 0))
  (let ((prefix (prefix trie))
        (suffixes (suffixes trie)))
    (cond
      (prefix
       (write-char #\Newline stream)
       (dotimes (i indent) (write-char #\Space stream))
        (format stream "(#\\~c" prefix))
      (t (write-char #\) stream)))

    (loop :for s :in suffixes
          :do (print-trie stream s (+ 2 indent))))

(defmethod print-object ((obj trie) stream)
    (print-unreadable-object (obj stream :type t)
        (let ((suffixes (suffixes obj)))
            (if suffixes
                (loop :for suffix :in (suffixes obj) 
                      :do (print-trie stream suffix))
                (write-string "NIL" stream)))))