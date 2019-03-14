; Worked with Rachel Hirmes

(defun MIN-2(A B)
    (cond
        ((not (numberp A)) 'ERROR)
        ((not (numberp B)) 'ERROR)
        ((<= A B) A)
        ((> B A) B)))
; Solution to Problem 1

(defun SAFE-AVG(A B)
    (cond
        ((not (numberp A)) NIL)
        ((not (numberp B)) NIL)
        (T (/ (+ A B) 2))))
; Solution to Problem 2

(defun ODD-GT-MILLION(A)
    (if (integerp A)
        (if (> A 1000000)
            (if (oddp A) T
                NIL))))
; Solution to Problem 3

(defun MULTIPLE-MEMBER(A B)
    (if (and (atom A) (listp B))
        (member A (rest (member A B)))
        (NIL)))
; Solution to Problem 4

(defun MONTH->INTEGER(A)
    (case A
        ('january 1)
        ('february 2)
        ('march 3)
        ('april 4)
        ('may 5)
        ('june 6)
        ('july 7)
        ('august 8)
        ('september 9)
        ('october 10)
        ('november 11)
        ('december 12)))
; Solution to Problem 5

(defun SCORE-GRADE(S)
    (cond
        ((not (numberp S)) NIL)
        ((>= S 90) 'A)
        ((>= S 87) 'A-)
        ((>= S 83) 'B+)
        ((>= S 80) 'B)
        ((>= S 77) 'B-)
        ((>= S 73) 'C+)
        ((>= S 70) 'C)
        ((>= S 60) 'D)
        ((< S 60) 'F)))
; Solution to Problem 6
        
(defun GT(A B)
    (and (numberp A) (numberp B) (> A B)))
; Solution to Problem 7

(defun SAME-PARTY(A B)
    (or (and (evenp A) (evenp B))
        (and (oddp A) (oddp B))))
; Solution to Problem 8

(defun SAFE-DIV(A B)
    (and (and (numberp A)
              (numberp B))
         (not (= 0 B))
         (/ A B)))
; Solution to Problem 9
