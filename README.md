## <center> Национальный исследовательский университет информационных технологий, механики и оптики </center> 
### <center> Факультет Программной Инженерии и Компьютерной Техники </center> 
----
 <br /> 
 <br />
 <br />

## <center> Лабораторная работа 2 </center>

### <center>Вариант "Prefix Tree Dict"</center>

### <center>«Функциональное программирование»</center>

<div style="text-align: right"> 

Работу выполнила:

Студентка группы P3212

Сенина Мария Михайловна

Преподаватель:

Пенской Александр Владимирович
</div>


<center>Санкт-Петербург</center>
<center>2023</center>

<div style="page-break-after: always; visibility: hidden">pagebreak</div>

# Цель
освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).
В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).
# Требования:

1. Функции:

    * добавление и удаление элементов;
    * фильтрация;
    * отображение (map);
    * свертки (левая и правая);
    * структура должна быть моноидом.


2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

## Содержание отчёта:
* титульный лист;
* требования к разработанному ПО;
* ключевые элементы реализации с минимальными комментариями;
* тесты, отчет инструмента тестирования, метрики;
* выводы (отзыв об использованных приёмах программирования).

# Реализация 

Я реализовала префиксное дерево на языке common lisp. 

## Интерфейс

`make-trie (&key initial-contents)` - создание дерева, принимает опциональные аргументы -- слова, на основе которых будет построено дерево
`insert (trie word)` - вставка слова в дерево, принимает аргументы -- дерево, в которое вставляем и слово, которое нужно вставить 
`search-trie (trie prefix)` - поиск слов с заданным префиксом, аргументы -- дерево и префикс
`print-trie (stream trie &optional (indent 0))` - вывод дерева в поток, аргументы -- дерево и поток
`words (trie &optional stack)` - вывод всех слов, на которых построено дерево
`map-trie (trie fun)` - фильтрация по всем словам дерева
`delete-trie (trie prefix)` - удаление всех детей у заданного узла
`lreduce-trie (trie fun)` - левая свёртка
`rreduce-trie (trie fun)` - правая свёртка
`sum-tries (trie-1 trie-2)` - функция сложения для того, чтобы структура была моноидом

## Тесты
Для юнит тестирования использовалась библиотека `(ql:quickload :lisp-unit)`
Для генерации строк для property based `(ql:quickload :check-it)`

### Юнит тестирование

Я реализовала фунцию сравнения содержимого файлов и далее для каждой операции сравнивала её вывод с тем, что написано в файле "с ответами":

```cl
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
```

Далее для каждого вида тестов я писала тест-обёртку принимающую на вход данные и вызывала её с asserts c с разными данными.

```cl
(defun creation-test(res-name answer-name initial-contents)
  (with-open-file (output res-name
                          :direction :output
                          :if-exists :supersede)
		  (trie:print-trie output (trie:make-trie :initial-contents initial-contents)))
  (compare-files res-name answer-name))
```

```cl
(lisp-unit:define-test test-1
		       (lisp-unit:assert-true (creation-test "test/test1" "test/test1-answers" '())))

(lisp-unit:define-test test-2
		       (lisp-unit:assert-true (creation-test "test/test2" "test/test2-answers" '("b" "a" "c"))))

(lisp-unit:define-test test-3
		       (lisp-unit:assert-true (creation-test "test/test3" "test/test3-answers" '("bac" "acd" "acdb"))))
```

### Property-based тестирование

Здесь я написала функцию сравнения деревьев:
```cl
(defun compare-tries (t1 t2)
  (not (set-exclusive-or (trie:words t1) (trie:words t2) :test #'string=)))
```
И генератор входных данных для создания деревьев. Это списки строк.

```cl
(defun gen-str (len)
  (check-it:generate (check-it:generator (string :min-length 2 :max-length len))))
```

К ним так же писались обёртки и они запускались lisp-unit

```cl
(defun sum-property-based ()
  (let ((lst1 (gen-str-list 4 5))
        (lst2 (gen-str-list 4 5)))
    (compare-tries (trie:sum-tries
                    (trie:make-trie :initial-contents lst1)
                    (trie:make-trie :initial-contents lst2))
                   (trie:sum-tries
                    (trie:make-trie :initial-contents lst2)
                    (trie:make-trie :initial-contents lst1)))))
```

```cl
(lisp-unit:define-test test-21
		       (lisp-unit:assert-true (sum-property-based)))
```

# Заключение

В этой лаборатороной работе я познакомилась c классами в common lisp. И его системами тестирования.
Первый раз попробовала подход property-based testing
