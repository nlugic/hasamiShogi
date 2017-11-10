(defun game (n)
  (gamePlay n 1 (* n 2) (* n 2) (initMatrix 1 n))
)

(defun gamePlay (n p xN oN li)
  (showMatrix n li)
)
(defun initMatrix (i n)
  (cond ((= i (1+ n)) '())
        ((< i 3) (cons (append (list i) (list (initRow 1 n 1))) (initMatrix (1+ i) n)))
        ((> i (- n 2)) (cons (append (list i) (list (initRow 1 n 2))) (initMatrix (1+ i) n)))
        ('t (cons (append (list i) (list (initRow 1 n 0))) (initMatrix (1+ i) n)))
))

(defun initRow (j n el)
  (cond ((= j (1+ n)) '())
        ('t (cons (list j el) (initRow (1+ j) n el)))
))

(defun showMatrix (n li)
(dotimes( g (1+ n))
            (cond ((= g 0) (format t "  "))
                  ('t (format t " ~d " g ))
))
  (dotimes (i (1+ n))
    (cond ((= i 0) (format t "   "))
                  ('t (print (indToChar i))
                         (dotimes (j (1+ n))
                           (cond ((> j 0) (format t "|~a|" (retLetter (getValue i j li))))
                                 ('t '" ")
                           
))) 
)))

(defun retLetter (el)
  (cond ((= el 0) "-")
        ((= el 1) "X")
        ('t "O"))
)

(defun charToInd (ch)
	(let 
		((abc '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
		(cond 
			((not (member ch abc)) '())
			(t (1+ (position ch abc)))
		)
	)
)

(defun indToChar (ind)
	(let 
		((abc '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
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
	(car (cdr (assoc j (car (cdr (assoc i mat))))))
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

