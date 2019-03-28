;worked with Rachel Hirmes
;Solution to 1
(defun index (N L)
	(if (endp L) 'ERROR
    	(if (= N 1) (car L) (index (- N 1) (cdr L)))))


;Solution to 2
(defun min-first (L)
	(if (= 1 (length L)) L
        (let ((X (min-first (cdr L))))
                (if (<= (car L) (car X))
                        L
                        (cons (car X) (cons (car L) (cdr X)))))))

;Solution to 3
(defun ssort (L)
	(if (= 1 (length L)) L
        (let* ((L1 (min-first L))
        (X (ssort (cdr L1))))
        (cons (car L1) X))))


;Solution to 4
(DEFUN QSORT (L)
	(IF (ENDP L)
	NIL
	(LET ((PL (PARTITION (CDR L) (CAR L))))
	(append (QSORT (car PL)) (cons (car L) (QSORT (cadr PL)))))))

;Solution to 5
(defun MERGE-LISTS (L1 L2)
	(COND
		((endp L1) L2)
		((endp L2) L1)
		((< (car L1) (car L2)) (cons (car L1) (MERGE-LISTS (cdr L1) L2)))
		(T (cons (car L2) (MERGE-LISTS (cdr L2) L1)))))

;Solution to 6
(defun MSORT (L)
	(if (= 1 (LENGTH L))
		L
		(LET ((L1 (SPLIT-LIST L)))
		(MERGE-LISTS (MSORT (CAR L1)) (MSORT (CADR L1))))))

;Solution to 7
(defun REMOVE-ADJ-DUPL (L)
	(if (ENDP L)
		L
		(LET ((X (REMOVE-ADJ-DUPL (CDR L))))
		(If (equal (CAR L) (CADR L)) X (CONS (CAR L) X)))))

;Solution to 8
(defun UNREPEATED-ELTS (L)
	(cond 
		((endp L) nil)
		((or (endp (cdr L)) (not (equal (car L) (cadr L))))
            	(cons (car L)(unrepeated-elts (cdr L))))
        ((or (endp (cddr L))(not (equal (car L)(caddr L))))
         (unrepeated-elts (cddr L)))
         (T (unrepeated-elts (cdr L)))))


; Solution to Problem 9
(defun REPEATED-ELTS (L)
    (cond ((endp L) nil)
           ((or (endp (cdr L)) (not (equal (car L) (cadr L))))
            (repeated-elts (cdr L)))
        ((or (endp (cddr L))(not (equal (car L)(caddr L))))
         (cons (car L) (repeated-elts (cddr L))))
         (T (repeated-elts (cdr L)))))


; Solution to Problem 10
(defun COUNT-REPETITIONS (L)
    (if (endp L)
        nil
       (let ((x (count-repetitions (cdr L))))
            (if (equal (car L) (cadr L))
                (cons (list (+ 1 (caar X)) (car L))
                      (cdr X))
                (cons (list 1 (car L)) X)))))


; Solution to Problem 11
(defun subset (f L)
    (if (endp L)
        nil
        (let ((X (subset f (cdr L))))
             (if (funcall f (car L))
                 (cons (car L) X)
                 X))))
; Solution to Problem 12i
(defun OUR-SOME (f L)
    (cond ((endp L) nil)
        ((funcall f (car L)) L)
        (T (our-some f (cdr L)))))
; Solution to Problem 12ii
(defun OUR-EVERY (f L)
    (if (endp L)
        T
        (and (funcall f (car L)) (our-every f (car L)))))


; Solution to Problem 13
(defun partition1 (f L P)
    (if (endp L)
        (list nil nil)
        (let ((X (partition1 f (cdr L) P)))
             (if (funcall f (car L) P)
                 (list (cons (car L) (car X)) (cadr X))
                 (list (car X) (cons (car L) (cadr X)))))))
(DEFUN QSORT1 (f L)
       (IF (ENDP L)
           NIL
           (LET ((PL (PARTITION1 f (CDR L) (CAR L))))
                (append (QSORT1 f (car PL)) (cons (car L) (QSORT1 f (cadr PL)))))))


; Solution to Problem 14
(defun foo (f L)
    (if (endp L)
        nil
        (append (list (cons (funcall f (car L)) (cdr L)))
                (mapcar (lambda (L1) (cons (car L) L1)) (foo f (cdr L))))))

; Solution to Problem 15
(defun TR-ADD (L result)
    (if (endp L)
        result
        (TR-ADD (cdr L) (+ (car L) result))))
(defun TR-MUL (L result)
    (if (endp L)
        result
        (TR-MUL (cdr L) (* (car L) result))))
(defun TR-FAC (L result)
    (if (zerop L)
        result
        (TR-FAC (- L 1) (* L result))))
(defun slow-primep (n)
    (if (= (mod (TR-FAC (- n 1) 1) n) (- n 1))
        T))


; Solution to Problem 16
(defun transpose1 (m)
    (if (endp (cdr M))
        (mapcar #'list (car M))
        (mapcar #'cons (car M) (transpose1 (cdr M)))))
(defun transpose2 (m)
    (if (endp (cdar M))
        (list (mapcar #'car M))
        (cons (mapcar #'car M) (transpose2 (mapcar #'cdr M)))))
(defun transpose3 (m)
    (apply #'mapcar #'list M))

