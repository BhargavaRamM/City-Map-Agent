;problem-1

(defun twice (lst1 lst2)
  (cond((or( null lst2) (null lst1)) 0) 
       ((= 2 (count lst1 lst2)) (+ 1 (twice (cdr lst1)  lst2)))
       (t (twice (cdr lst1) lst2))))
	 
(defun count (lst1 lst2)
  (cond ((or (null lst1) (null lst2)) 0)
	((or ( > (car lst1) (car lst2)) (< (car lst1) (car lst2))) (count lst1 (cdr lst2))) 
	((= (car lst1) (car lst2)) (+ 1 (count lst1  (cdr lst2))))))
;-------------------------------------------------------------------------------------------------------------
;problem-2

(defun neg_first (lst)
  (append (negative-first lst) (positive-first lst))))

(defun negative-first (lst)
  (cond ((null lst) ())
        (( >= (car lst) 0 ) (negative-first (cdr lst)))
        (t (cons (car lst) (negative-first( cdr lst))))))
(defun positive-first (lst)
  (cond ((null lst) ())
        (( < (car lst) 0) (positive-first (cdr lst)))
        (t (cons (car lst)(positive-first (cdr lst))))))
;----------------------------------------------------------------------------------------------------------------
;problem-3

(defun postfix (lst)
  (cond ((null lst ) ())
        ((and (atom (car lst))(not (numberp (car lst)))) (append (postfix(cdr lst))(list(car lst))))
        ((listp (car lst)) (cons (postfix(car lst)) (postfix (cdr lst))))
        ((numberp(car lst)) (cons (car lst) (postfix (cdr lst))))))
;----------------------------------------------------------------------------------------------------------------
;problem-4

(defun repeat (lst)
   (cond ((null lst) ())
         (t (reverse(revx (reverse lst)))))))
(defun revx (lst)
  (cond ((null lst) ())
        ((= (length lst) 1) (list (rep (car lst) 1)))
        ((> (length lst) 1)  (append (reverse (list (rep (car lst) (length lst)))) (revx (cdr lst))))))
(defun rep (i j)
  (cond  ((= j 1) (list i))
         ((> j 1) (cons i (rep i (- j 1))))))

;----------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------
;problem-5

(defun transpose_t (lst)
  (cond ((null lst) ())
        ((null (car lst)) ())
        (t (cons (transpose lst) (transpose_t(mapcar 'cdr lst))))))

(defun transpose (lst)
(cond ((null lst) ())
      ((null (car lst)) ())
      ((listp (car lst)) (cons (caar lst) (transpose (cdr lst))))
      (t (transpose(mapcar 'cdr lst)))))

;---------------------------------------------------------------------------------------------------------
;problem-6

(defun graft (lst1 lst2)
  (cond ((null lst1) lst2)
        ((null lst2) ())
        ((atom (car lst2)) (cons (car lst2 )(graft lst1 (cdr lst2))))
        ((listp (car lst2)) (cons(append (graft lst1 (car lst2)) lst1) (graft lst1 (cdr lst2))))))

;-------------------------------------------------------------------------------------------------------------------
;problem-7

(defun outside (i j lst)
  (cond ((null lst) ())
        ((null (cddr lst)) ())
        ((and (null (cadr lst)) (or (> i (car lst)) (< j (car lst)))) (append (list(car lst)) (outside i j (caddr lst))))
        ((and(null (cadr lst)) (and (< i (car lst)) (> j (car lst)))) (outside i j (caddr lst)))
        ( (null(cddr lst))  (append (outside i j (cadr lst))))
        ((and (or (listp (cadr lst)) (listp (cddr lst))) (or (> i (car lst)) (< j (car lst)))) (append (outside i j (cadr lst)) (list(car lst)) (outside i j (caddr lst))))
        ((and (listp (cadr lst)) (and (< i (car lst)) (> j (car lst)))) (append (outside i j (cadr lst)) (outside i j (caddr lst))))
        ((listp (cddr lst)) (append (outside i j (cadr lst))(list(car lst)) (outside i j (cddr lst))))))
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;problem-8

(defun multiply (lst1 lst2)
  (cond ((or (null lst1) (null lst2)) ())
        ((and (numberp lst1) (numberp lst2)) (* lst1 lst2))
        ((and(numberp lst1) (listp lst2) (eq (car lst2) +)) (append (car lst2) (multiply  lst1 (cadr lst2)) (multiply lst1 (caddr lst2))))
        ((and(numberp lst1) (listp lst2) (eq (car lst2) *)) (append (car lst2) (multiply  lst1 (cadr lst2)) (caddr lst2)))
        ((and (listp lst1) (numberp lst2) (eq (car lst1) +)) (append (car lst1) (multiply (cadr lst1) lst2) (multiply (caddr lst1) lst2))) 
        ((and (listp lst1) (numberp lst2) (eq (car lst1)*)) (append (car lst1) (multiply (cadr lst1) lst2) (caddr lst1)))
        ((and(and (listp lst1) (listp lst2) (or (eq (car lst1) +) (eq (car lst2) +)))) (poly-mul lst1 lst2))))
(defun poly-mul (lst1 lst2)
 (cond ((or (null lst1) (null lst2)) ()) 
        ((and (eq (car lst1) +)(eq (car lst2) +))  (append (list(car lst1)) (multiply (cdr lst1) (cdr lst2))))
        ((or(and (eq (car lst1) +) (eq (car lst2) *)) (and (eq (car lst1) *) (eq (car lst2) +))) (append (list +) (multiply (cdr lst1) (cdr lst2))))
        ((or(and (eq (car lst1) +) (eq (car lst2) -)) (and (eq (car lst1) -) (eq (car lst2) +))) (append (list -) (multiply (cdr lst1) (cdr lst2))))
        ((or(and (eq (car lst1) -) (eq (car lst2) *)) (and (eq (car lst1) *) (eq (car lst2) -))) (append (list -) (multiply (cdr lst1) (cdr lst2))))
        ((and (eq (car lst1) -) (eq (car lst2) -)) (append (list(car lst1)) (multiply (cdr lst1) (cdr lst2))))
        ((and (eq (car lst1) *) (eq (car lst2) *)) (append (list(car lst1)) (multiply (cdr lst1) (cdr lst2))))
        ((and (eq (car lst1) %) (eq (car lst2) %)) (append (list(car lst1)) (cadr lst2) (+ (caddr lst1) (caddr lst2))))))


;-----------------------------------------------------------------------------------------------------------------------
;problem-9

(defun inherit-get (symbol property)
  (cond ((not(null (get symbol property))) (get symbol property))
	((null(get symbol 'isa)) ())
        ((null (get symbol property)) (inherit-get (get symbol 'isa) property))
        ((null(inherit-get symbol property)) ())))

    
;-----------------------------------------------------------------------------------------------------------------        
;problem-10    

(defun full-inherit-get (symbol-lst property)
  (cond ((null symbol-lst) ())
	((and(atom symbol-lst) (not(null(get symbol-lst property)))) (get symbol-lst property))
        ((and (atom symbol-lst) (get symbol-lst 'isa)) (full-inherit-get (get symbol-lst 'isa) property))
        ((get (car symbol-lst) property) (get (car symbol-lst) property))
        (t (full-inherit-get (append (cdr symbol-lst) (get (car symbol-lst) 'isa)) property))))

