(defpackage :trie
  (:use :cl)
  (:export :make-trie
	   :insert
	   :search-trie
	   :print-trie
	   :words
	   :map-trie
	   :delete-trie
	   :lreduce-trie
	   :rreduce-trie
	   :sum-tries
	   ))

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
  ;; чистая функция
  ;; если слово положительной длины оставляем -- иначе запишем 0
  (let* ((char (and (plusp (length word))
                    (char word 0)))
         ;; запишем в next
         (next (and char
                    ;; найдём среди детей ребёнка с префиксом char
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
			 ;; в суффиксы этого узла все остальные суфиксы слова
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

(defun map-trie(trie fun)
  (mapcar fun (trie:words trie)))

(defun lreduce-trie(trie fun)
  (reduce fun (trie:words trie)))

(defun rreduce-trie(trie fun)
  (reduce fun (trie:words trie) :from-end t))

(defun search-trie (trie prefix)
  (if (string= prefix "")
      ;; если префикс -- пустая строка, выводим все слова дерева
      (words trie)
    ;; иначе ищем, есть ли такой префикс в нашем дереве
    (multiple-value-bind (trunk rest) (trunk trie prefix)
			 ;; если есть полное совпадение, т.е. rest = nil
			 (if (null rest)
			     (let ((len (1- (length prefix))))
			       (mapcar (lambda (x) (concatenate 'string (subseq prefix 0 len) x))
				       (words trunk))))))
  )

(defun delete-trie (trie prefix)
  (if (string= prefix "")
      ;; если префикс -- пустая строка, выводим пустое дерево
      (progn (setf (suffixes trie) nil) trie)
    ;; ищем, есть ли такой префикс в нашем дереве
    (multiple-value-bind (trunk rest) (trunk trie prefix)
			 ;; если есть полное совпадение, т.е. rest = nil
			 (if (null rest)
					; присваеваем суфиксам этой ноды пустой список
			     (setf (suffixes trunk) nil)) trie)))

(defun sum-tries (trie-1 trie-2)
  (let ((words-1 (words trie-1))
        (words-2 (words trie-2)))
    (let ((new-words (copy-list words-1)))
      (dolist (w words-2)
        (cond ((null (member w words-1 :test #'string=))
               (push w new-words))))
      (make-trie :initial-contents new-words))))

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
          :do (print-trie stream s (+ 2 indent)))))

(defmethod print-object ((obj trie) stream)
  (print-unreadable-object (obj stream :type t)
			   (let ((suffixes (suffixes obj)))
			     (if suffixes
				 (loop :for suffix :in (suffixes obj)
				       :do (print-trie stream suffix))
			       (write-string "NIL" stream)))))
