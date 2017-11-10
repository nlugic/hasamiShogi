(defun startGame()
  (game (startGameInternNRead '()) (startGameInternPRead '()))
)


(defun startGameInternNRead(ERROR)
  (progn 
    (cond ((not (null ERROR)) (format t "~%Pogresno ste uneli Velecinu Table: "))
    ('t (format t "~%Unesite Velecinu Table: ")))
    (let* ((N (read)))
           (cond ((and (> N 7) (< N 26)) N)
                 ('t (startGameInternNRead 't))
                 )
           
)
))

(defun startGameInternPRead(ERROR)
  (progn 
    (cond ((not (null ERROR)) (format t "~%Pogresno ste uneli Prvog Igraca (1 X) (2 O): "))
    ('t (format t "~%Unesite Prvog Igraca (1 X) (2 O): ")))
    (let* ((N (read)))
           (cond ((or (= N 1) (= N 2)) N)
                 ('t (startGameInternPRead 't))
                 )
           
)
))



(defun game (n p)
  (gamePlay (wrapMatrix p (* n 2) (* n 2) n (initMatrix 1 n)))
)

(defun gamePlay(mat)
  (showMatrix (getn mat) mat)
  (gamePlay (setp (playTurn mat (promptForTurn (getp mat) '() mat)) (logxor (getp mat) 3 )))
  
)
(defun playTurn(mat turn)
(setFigure (car (cdr turn)) (getP mat) (unsetFigure (car turn) mat))
)
(defun wrapMatrix(p brX brO n mat)
  (append (list (cons 64 (list (cons n (cons p (cons brX (cons brO (cons '0 '())))))))) mat)
 
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
(dotimes (g (1+ n))
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

(defun getN (mat)
  (caadr (assoc 64 mat))
)

(defun getP (mat)
  (cadadr (assoc 64 mat))
)

(defun getNX (mat)
  (caddadr (assoc 64 mat))
)

(defun getNO (mat)
  (cadddadr (assoc 64 mat))
)

(defun getFinished (mat)
  (caddddadr (assoc 64 mat))
)
(defun setP(mat p)
 (append (list (cons 64 (list (cons (car(car (cdr (assoc 64 mat)))) (cons p (cdr (cdr (car (cdr (assoc 64 mat)))))))))) (cdr mat))

)
(defun promptForTurn (p err mat)
  (progn
    (cond
     ((not (null err)) (format t "~%Potez nije validan!"))
     (t (format t "~%~a je na potezu:" p))
     )
    (let* ((turnList (list (read) (read) (read) (read))) (turn (makeTurn turnList mat)))
      (cond
       ((null turn) (promptForTurn p t mat))
       (t turn)
       )
      )
    )
)

(defun makeTurn (turn mat)
	(cond
		((turnSyntaxInvalid turn (getN mat) (getP mat) mat) '())
		(t 
			(list 
				(list (charToInd (car turn)) (cadr turn)) 
				(list (charToInd (caddr turn)) (cadddr turn))
			)
		)
	)
)

(defun turnSyntaxInvalid (turn n p mat)
  (let ((rowF (charToInd (car turn)))
        (rowT (charToInd (caddr turn)))
        (colF (cadr turn))
        (colT (cadddr turn)))
    (cond
     ((< (length turn) 4) t)
     ((equal (equal colF colT) (equal rowF rowT)) t)
     ((> colF n) t)
     ((< colF 1) t)
     ((> rowF n) t)
     ((< rowF 1) t)
     ((> colT n) t)
     ((< colT 1) t)
     ((> rowT n) t)
     ((< rowT 1) t)
     ((not (= (getValue rowF colF mat) p)) t)
     ((> (getValue rowT colT mat) 0) t)
     )
    )
)

(defun getValue (i j mat)
	(car (cdr (assoc j (car (cdr (assoc i mat))))))
)

(defun unsetFigure (pos mat)
	(cond
		((null mat) '())
		((equal (car pos) (caar mat)) (cons (cons (caar mat) (list (unsetFromRow (cadr pos) (cadar mat)))) (cdr mat)))
		(t (cons (car mat) (unsetFigure pos (cdr mat))))
	)
)

(defun unsetFromRow (ind row)
	(cond
		((null row) '())
		((equal ind (caar row)) (cons (list ind 0) (cdr row)))
		(t (cons (car row) (unsetFromRow ind (cdr row))))
	)
)

(defun setFigure (pos val mat)
	(cond
		((null mat) '())
		((equal (car pos) (caar mat)) (cons (list (caar mat) (setToRow (cadr pos) val (cadar mat))) (cdr mat)))
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

