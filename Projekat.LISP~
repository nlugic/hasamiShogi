(defun Game(n)
  (GamePlay n 1 (* n 2) (* n 2) (initMatrix 0 n))
)

(defun GamePlay(n p xN oN li)
  (ShowMatrix2 n li)
)
(defun initMatrix(i n)
  (cond ((= i n) '())
        ((< i 2) (cons (append (list i) (list (initRow 0 n 1))) (initMatrix (+ i 1) n)))
        ((> i (- n 3)) (cons (append (list i) (list (initRow 0 n 2))) (initMatrix (+ i 1) n)))
        ('t (cons (append (list i) (list (initRow 0 n 0))) (initMatrix (+ i 1) n)))
))

(defun initRow(j n el)
  (cond ((= j n) '())
        ('t (cons (list j el) (initRow (+ j 1) n el)))
))

(defun ShowMatrix(i li n)
  (cond ((= i n) '())
        ('t (print i )(ShowRow 0 (car (cdr (assoc i li))) n) (ShowMatrix (+ i 1) li n))
))

(defun ShowRow(j li n)
  (cond ((= j n) '())
        ('t (format t "~D~%" (car (cdr (assoc j li)))) (ShowRow (+ j 1) li n))
        ('t (write "(car (cdr (assoc j li))) -" ) (ShowRow (+ j 1) li n))
))

(defun ShowMatrix2(n li)
(dotimes( g (+ 1 n))
            (cond ((= g 0) (format t "  "))
                  ('t (format t " ~d " (- g 1)))
)
)
  (dotimes(i n)
    (print i)
    (dotimes(j n)
      (format t "|~a|" (RetLetter(car (cdr (assoc j (car (cdr (assoc i li))))))))
)
)
)

(defun RetLetter(el)
  (cond ((= el 0) "-")
        ((= el 1) "X")
        ('t "O"))
)


(defun charToInd (ch)
	(let 
		((abc '(A B C D E F G H I J K L M N O)))
		(cond 
			((not (member ch abc)) '())
			(t (1+ (position ch abc)))
		)
	)
)

(defun indToChar (ind)
	(let 
		((abc '(A B C D E F G H I J K L M N O)))
		(cond 
			((< ind 0) '())
			((= ind 0) '())
			((> ind (length abc)) '())
			(t (nth (1- ind) abc))
		)
	)
)

(defun makeTurn (turn)
	(cond
		((turnSyntaxInvalid turn) '())
		(t 
			(list 
				(list (charToInd (car turn)) (cadr turn)) 
				(list (charToInd (caddr turn)) (cadddr turn))
			)
		)
	)
)

(defun turnSyntaxInvalid (turn)
	(cond
		((< (length turn) 4) t)
		((equal (equal (car turn) (caddr turn)) (equal (cadr turn) (cadddr turn))) t)
		((not (charToInd (car turn))) t)
		((not (indToChar (cadr turn))) t)
		((not (charToInd (caddr turn))) t)
		((not (indToChar (cadddr turn))) t)
		(t '())
	)
)

(defun getValue (i j mat)
	(assoc j (assoc i mat))
)

(defun unsetFigure (pos mat)
	(cond
		((null mat) '())
		((equal (car pos) (caar mat)) (cons (cons (caar mat) (unsetFromRow (cdr pos) (cdar mat))) (cdr mat)))
		(t (cons (car mat) (unsetFigure pos (cdr mat))))
	)
)

(defun unsetFromRow (ind row)
	(cond
		((null row) '())
		((equal ind (caar row)) (cons (list ind '-) (cdr row)))
		(t (cons (car row) (unsetFromRow ind (cdr row))))
	)
)

(defun setFigure (pos val mat)
	(cond
		((null mat) '())
		((equal (car pos) (caar mat)) (cons (cons (caar mat) (setToRow (cdr pos) (cdar mat) val)) (cdr mat)))
		(t (cons (car mat) (setFigure pos val (cdr mat))))
	)
)

(defun setToRow (ind val row)
	(cond
		((null row) '())
		((equal ind (caar row)) (cons (list ind val) (cdr row)))
		(t (cons (car row) (setToRow ind val (cdr row))))
	)
)

