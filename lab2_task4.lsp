;Написать программу объединения двух отсортированных списков в один. При этом порядок
;сортировки в списке-результате должен сохраняться.


(defun unite-sorted-lists(lst1 lst2 lst3)
  (cond
    ((and (null lst1) (null lst2) )lst3)
    ((and (null lst1) (not (null lst2))) (append lst3 lst2))
    ((and (not (null lst1)) (null lst2)) (append lst3 lst1))
    ((<= (car lst1) (car lst2)) (unite-sorted-lists (cdr lst1) lst2 (append lst3 (list(car lst1)))))
    ((<= (car lst2) (car lst1)) (unite-sorted-lists (cdr lst2) lst1 (append lst3 (list(car lst2)))))
    (t lst3)
  )
)

(defparameter a_test `(1 23))
(defparameter c_test `(1 2 3 111 222 3444))
(defparameter b_test `(22 33))
(defparameter res `())

(write (unite-sorted-lists a_test c_test res))
