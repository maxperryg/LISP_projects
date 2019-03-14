; Worked with Rachel Hirmes

;Solution to problem 1
(defun sum (L)
	(if (endp L) 0
		(+ (sum (cdr L)) (car l))))

; Solution to 2
(defun neg-nums (L)
        (if (endp L) NIL 
        (let ((X (neg-nums (cdr L))))
                (if (< (car L) 0) (cons (car L) X) X))))

; Solution to 3
(defun inc-list-2 (L N)
	(if (endp L) L
                (cons (+ n (car L)) (inc-list-2 (cdr L) N))))

; Solution to 4
(defun insert (N L)
	(if (endp L) (list N)
		(if (<= N (car L))
			(cons N L)
                        (cons (car L) (insert N (cdr L))))))

; Solution to 5
(defun isort (L)
	(if (endp L) NIL
               (insert (car L) (isort (cdr L)))))

;solution to 6
(defun split-list (L)
    (if (endp L) 
        L
        (let ((X (split-list (cdr L))))
            (list (cons (car L) (cadr X)) (car x)))))

; Solution to 7
(defun partition (L P)
    (if (endp L)
        (list NIL NIL)
        (let ((X (partition (cdr L) P)))
            (if (< (car L) p)
                (list (cons (car L) (car X)) (cadr X))
                (list (car X) (cons (car L) (cadr X)))))))

; Solution to 8
(DEFUN POS (E L)
 	(COND ((ENDP L) 0)
 		((EQUAL E (CAR L)) 1)
 			(T (LET ((X (POS E (CDR L))))
				(if (zerop x) 0 
				(+ x 1)))))) 

; Solution to 9
(defun SPLIT-NUMS (N)
    (if (zerop N) (list '(0) nil)
        (let ((X (split-nums(- N 1))))
              (if (evenp N)
                  (list (cons N (car X))
                        (cadr X))
                  (list (car X)
                        (cons N (cadr X)))))))

; Solution to 10
(defun SET-UNION (L1 L2)
	(if (endp L1)
		L2
		(LET ((X (SET-UNION (CDR L1) L2)))
			(if (MEMBER (CAR L1) X)
				X
				(cons (CAR L1) X)))))

; Solution to 11
(defun SET-REMOVE (N S)
	(if (endp S)
		NIL
		(let ((X (SET-REMOVE N (CDR S))))
			(if (Equal N (CAR S))
				X
				(CONS (CAR S) X)))))


; Solution to 12
(defun SET-EXCL-UNION (S1 S2)
    (if (endp S1) S2
        (LET ((X (SET-EXCL-UNION (CDR S1) S2)))
            (if (MEMBER (CAR S1) X)
                (SET-REMOVE (CAR S1) X)
                (Cons (CAR S1) X)))))

; Solution to 13
(defun singletons (e)
    (if (endp e) e
        (let ((X (singletons (cdr e))))
             (if (member (car e) (cdr e))
                 (set-remove (car e) x)
                 (cons (car e) x)))))

