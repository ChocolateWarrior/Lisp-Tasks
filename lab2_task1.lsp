(defun member(a s)
  (cond
    ((null (atom a)) nil)
    ((null (listp s)) nil)
    ((null s) nil)
    ((equal a (car s)) t)
    (t (member a (cdr s)))
  )
)

(defun trim-to-element(a lst1)
  (cond
    ((equal a (car lst1)) (cdr lst1))
    (t (trim-to-element a (cdr lst1)))
  )
)

(defun unio(s1 s2)
  (cond
    ((null s1) s2)
    ((null s2) s1)
    ((member (car s2) s1) (unio s1 (cddr s2)))
    (t (cons (car s2) (unio s1 (cdr s2))))
  )
)

(defun intersect(s1 s2)
  (cond
    ((null s1) nil)
    ((null s2) nil)
    ((member (car s2) s1) (unio (list (car s2)) (intersect s1 (cdr s2))))
    (t (intersect s1 (cdr s2)))
  )
)

(defun rev (l)
  (cond
    ((null l) '())
    (t (append (rev (cdr l)) (list (car l))))
  )
)

(defun eq-atom-order(lst1 lst2)
  (cond
    ((and (null lst1) (null lst2) )T)
    ((member (car (rev(intersect lst1 lst2))) lst1)
    (eq-atom-order (cdr lst1) (trim-to-element (car (rev (intersect lst1 lst2))) lst1)))
    (t lst2)
  )
)

(print (eq-atom-order a_test b_test))
(print (eq-atom-order a_test c_test))
(print (trim-to-element 5 a_test))
(print (not( member (car a_test) b_test)))
(print (rev (intersect a_test b_test)))

(defparameter a_test `(1 2 3 4 5 6 8 7 9))
(defparameter c_test `(1 2 3 111 222 3444))
(defparameter b_test `(22 33 44 5 6 9 7 3 4))
