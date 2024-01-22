(defpackage :trie
  (:use :cl)
  (:export :make-trie
	   :insert
	   :search-trie
	   :print-trie
	   :list-words
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

(defun copy (trie)
	(let ((new-trie (make-instance 'trie)))
		(if (null trie) ()
				(progn
					(setf (prefix new-trie) (prefix trie))
					(setf (suffixes new-trie) 
							(mapcar #'(lambda (s) (copy s)) (suffixes trie)))))
	new-trie))

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
	(let ((ctrie (copy trie)))
	;; находим максимальный префикс, записываем его в rest
	(multiple-value-bind (trunk rest) (trunk ctrie word)
		(cond
			((not (null rest))
				;; если суфикс не нулевой (такого значения ещё нет) добавляем
				;; в суффиксы этого узла все остальные суфиксы слова
				; находим максимальный префикс, записываем его в rest
				(push (make-suffix rest) (suffixes trunk))))
	ctrie)))

;; конструктор
(defun make-trie (&key initial-contents)
	(create-trie (make-instance 'trie) initial-contents))

(defun create-trie (trie initial-contents)
	(if (null initial-contents) trie
		(if (null (cdr initial-contents))
			(insert trie (car initial-contents))
			(create-trie (insert trie (car initial-contents)) (cdr initial-contents)))))

(defun words (trie &optional stack)
  (let ((prefix (prefix trie))
        (suffixes (suffixes trie)))
    (cond
     ;; если нода нулевая -- это конец
     ((and (null prefix) (null suffixes))
      (if stack
          ;; преобразуем в строчку слово, до которого дошли
          (list (coerce (reverse stack) 'string))))
     ;; в цикле ходим по всем суфиксам из этой ноды и рекурсивно ждём от них все слова
     (t (loop :for suffix :in suffixes :append (words suffix (if prefix (cons prefix stack) stack)))))))

(defun list-words (trie)
	(if (null trie)
		nil
		(words trie)))

(defun map-trie(trie fun)
  (mapcar fun (list-words trie)))

(defun lreduce-trie(trie fun)
  (reduce fun (list-words trie)))

(defun rreduce-trie(trie fun)
  (reduce fun (list-words trie) :from-end t))

(defun search-trie (trie prefix)
  (if (string= prefix "")
      ;; если префикс -- пустая строка, выводим все слова дерева
      (list-words trie)
    ;; иначе ищем, есть ли такой префикс в нашем дереве
    (multiple-value-bind (trunk rest) (trunk trie prefix)
			 ;; если есть полное совпадение, т.е. rest = nil
			 (if (null rest)
			     (let ((len (1- (length prefix))))
			       (mapcar (lambda (x) (concatenate 'string (subseq prefix 0 len) x))
				       (list-words trunk))))))
  )

(defun delete-trie (trie prefix)
	(let ((ctrie (copy trie)))
		(if (string= prefix "")
			;; если префикс -- пустая строка, выводим пустое дерево
			(setf (suffixes ctrie) nil)
			;; ищем, есть ли такой префикс в нашем дереве
			(multiple-value-bind (trunk rest) (trunk ctrie prefix)
					;; если есть полное совпадение, т.е. rest = nil
					(if (null rest)
							; присваеваем суфиксам этой ноды пустой список
						(setf (suffixes trunk) nil))))
	ctrie))

(defun sum-tries (trie-1 trie-2)
  (let ((words-1 (list-words trie-1))
        (words-2 (list-words trie-2)))
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
