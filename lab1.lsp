;;;;                                  LAB#1-------------------------------------
; Author:Mark Zubach, IP-71, KPI, FICT------------------------------------------
; Variant 10--------------------------------------------------------------------
; TASK1-------------------------------------------------------------------------
; Описать неименованную функцию для объединения голов трех списков в один
; список, исходные данные взять из таблицы 4.

((lambda (x y z)
(LIST (CAR z) (CAR x) (CAR y))
)
'(FIR SED (1 2 3) (5) ()) '(H J U K (L M N) (D E L)) '(4 5(6 7)))

; TASK2-------------------------------------------------------------------------
; Описать именованную функцию для создания нового списка из элементов
; нескольких исходных списков. В качестве исходных списков использовать
; списки таблицы 4. Номера элементов списков взять в таблице 5.

(DEFUN TASK2 (G H J)
(LIST (CADDDR G)(CADDDR H)(CADDR J)))
(TASK2 '(FIR SED (1 2 3) (5) ()) '(H J U K (L M N) (D E L)) '(4 5(6 7)))

; TASK3-------------------------------------------------------------------------
; Написать функцию которая для аргумента-списка формирует список результат:
; если первый и последний элемнты списка-аргумента - четные, положительные,
; целые числа, то включить в список-результат первым элтом - квадрат последнего
; вторым - четвертую степень первого; в противном случае сформировать список из
; первого и последнего элементов.

(DEFUN TASK3 (lst)
(SETQ e1 (CAR lst))
(SETQ e2 (FIRST(LAST lst)))
(SETQ l2 (expt e2 2))
(SETQ l1 (expt e1 4))
(if
(and (evenp e1) (evenp 2) (plusp e1) (plusp e2) (integerp e1) (integerp e2))
(LIST l2 l1)
(LIST e1 e2)
)
)
(TASK3 '(2 6 3 4 5 6 7 8 9 .6))
(TASK3 '(4 5 6 7))