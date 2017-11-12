
(defun startGame ()
	(game (startGameInternNRead '()) (startGameInternPRead '()))
)

(defun startGameInternNRead (err)
	(progn 
		(cond
			((not (null err)) (format t "~%Pogresno ste uneli velicinu Table: "))
			('t (format t "~%Unesite velicinu Table: "))
		)
		(let*
			((N (read)))
			(cond
				((and (> N 6) (< N 27)) N)
				('t (startGameInternNRead 't))
			)
		)
	)
)

(defun startGameInternPRead (err)
	(progn 
		(cond
			((not (null err)) (format t "~%Pogresno ste uneli Prvog Igraca (1 X) (2 O): "))
			('t (format t "~%Unesite Prvog Igraca (1 X) (2 O): "))
		)
		(let*
			((N (read)))
			(cond 
				((or (= N 1) (= N 2)) N)
				('t (startGameInternPRead 't))
			)
		)
	)
)

(defun game (n p)
	(gamePlay (wrapMatrix p (* n 2) (* n 2) n (initMatrix 1 n)))
)

(defun gamePlay (mat)
	(showMatrix (getN mat) mat)
	(let*
		(
			(currP (getP mat))
			(nextP (logxor currP 3))
			(move (promptForTurn currP '() mat))
			(matrix (sandwich (caadr move) (cadadr move) currP nextP (playTurn mat move) '0))
		)
		(cond
			((isWin (caadr move) (cadadr move) currP matrix (getN matrix)) (write "you win")) 
			('t (gamePlay (setP matrix nextP)))
		)
	)
)

(defun playTurn(mat turn)
	(setFigure (cadr turn) (getP mat) (unsetFigure (car turn) mat))
)

(defun wrapMatrix(p brX brO n mat)
	(append (list (cons 64 (list (cons n (cons p (cons brX (cons brO (cons '0 '())))))))) mat)
)  

(defun initMatrix (i n)
	(cond
		((= i (1+ n)) '())
		((< i 3) (cons (append (list i) (list (initRow 1 n 1))) (initMatrix (1+ i) n)))
		((> i (- n 2)) (cons (append (list i) (list (initRow 1 n 2))) (initMatrix (1+ i) n)))
		('t (cons (append (list i) (list (initRow 1 n 0))) (initMatrix (1+ i) n)))
	)
)

(defun initRow (j n el)
	(cond
		((= j (1+ n)) '())
		('t (cons (list j el) (initRow (1+ j) n el)))
	)
)

(defun showMatrix (n li)
	(dotimes (g (1+ n))
		(cond
			((= g 0) (format t "  "))
			('t (format t " ~d " g ))
		)
	)
	(dotimes (i (1+ n))
		(cond
			((= i 0) (format t "   "))
			('t
				(print (indToChar i))
				(dotimes (j (1+ n))
					(cond
						((> j 0) (format t "|~a|" (getLetter (getValue i j li))))
						('t '" ")
					)
				)
			) 
		)
	)
)

(defun getLetter (p)
	(cond
		((= p 0) "-")
		((= p 1) "X")
		('t "O")
	)
)

(defun charToInd (ch)
	(let
		((chCode (char-code (coerce ch 'character))))
		(cond
			((> chCode 90) '())
			((< chCode 65) '())
			('t (- chCode 64))
		)
	)
)

(defun indToChar (ind)
	(cond 
		((< ind 0) '())
		((= ind 0) '())
		((> ind 26) '())
		('t (car (list (intern (string (code-char (+ ind 64)))))))
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

(defun setP (mat p)
	(append (list (cons 64 (list (cons (caadr (assoc 64 mat)) (cons p (cddadr (assoc 64 mat))))))) (cdr mat))
)

(defun promptForTurn (p err mat)
	(progn
		(cond
			((not (null err)) (format t "~%Potez nije validan!"))
			('t (format t "~%~a je na potezu:" p))
		)
		(let*
			((turnList (readList (read-line))) (turnStruct (makeTurn turnList mat)))
			(cond
				((null turnStruct) (promptForTurn p 't mat))
				('t turnStruct)
			)
		)
	)
)

(defun makeTurn (turn mat)
	(cond
		((turnInvalid turn (getN mat) (getP mat) mat) '())
		('t (makeTurnStruct turn))
	)
)

(defun makeTurnStruct (turn)
	(list 
		(list (charToInd (car turn)) (cadr turn)) 
		(list (charToInd (caddr turn)) (cadddr turn))
	)
)

(defun turnInvalid (turn n p mat)
	(let*
		(
			(turnStruct (makeTurnStruct turn))
			(rowF (car turnStruct))
			(rowT (caddr turnStruct))
			(colF (cadr turnStruct))
			(colT (cadddr turnStruct))
		)
		(cond
			((< (length turnStruct) 4) t)
			((equal (equal colF colT) (equal rowF rowT)) t)
			((or (> colF n) (< colF 1)) t)
			((or (> rowF n) (< rowF 1)) t)
			((or (> colT n) (< colT 1)) t)
			((or (> rowT n) (< rowT 1)) t)
			((not (= (getValue rowF colF mat) p)) t)
			((> (getValue rowT colT mat) 0) t)
			((turnBlocked turnStruct p mat) t)
			(t '())
		)
	)
)

(defun turnBlocked (turnStruct p mat)
	(let*
		((offset (turnOffset turnStruct)) (offsetH (cadr offset)) (offsetV (car offset)))
		(cond
			((not (= offsetH 0)) (horizontalTurnBlocked (signum offsetH) offsetH turnStruct p mat))
			(t (verticalTurnBlocked (signum offsetV) offsetV turnStruct p mat))
		)
	)
)

(defun horizontalTurnBlocked (currMove offset turnStruct p mat)
	(let
		((x (caar turnStruct)) (y (+ (cadar turnStruct) currMove)))
		(cond
			((= (- (abs currMove) (abs offset)) 1)  '())
			((and (= (abs offset) 2) (= (getValue x y mat) (logxor p 3)))
				(if (< currMove 0)
					(horizontalTurnBlocked (1- currMove) offset turnStruct p mat)
					(horizontalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
			((> (getValue x y mat) 0) t)
			(t 	(if (< currMove 0)
					(horizontalTurnBlocked (1- currMove) offset turnStruct p mat)
					(horizontalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
		)
	)
)

(defun verticalTurnBlocked (currMove offset turnStruct p mat)
	(let
		((x (+ (caar turnStruct) currMove)) (y (cadar turnStruct)))
		(cond
			((= (- (abs currMove) (abs offset)) 1) '())
			((and (= (abs offset) 2) (= (getValue x y mat) (logxor p 3)))
				(if (< currMove 0)
					(verticalTurnBlocked (1- currMove) offset turnStruct p mat)
					(verticalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
			((> (getValue x y mat) 0) t)
			(t 	(if (< currMove 0)
					(verticalTurnBlocked (1- currMove) offset turnStruct p mat)
					(verticalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
		)
	)
)

(defun turnOffset (turnStruct)
	(list (- (caadr turnStruct) (caar turnStruct)) (- (cadadr turnStruct) (cadar turnStruct)))
)

(defun getValue (i j mat)
	(cadr (assoc j (cadr (assoc i mat))))
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

(defun isSandwichVertical (i j player1 player2 mat n direction)
	(cond 
		((= direction 0) (list (isSandwichVertical (1- i) j player1 player2 mat n '1) (isSandwichVertical (1+ i) j player1 player2 mat n -1)))
		((< direction 0)
			(cond
				((equal (getValue i j mat) player2) (isSandwichVertical (1+ i) j player1 player2 mat n -1))
				((equal (getValue i j mat) player1) i)
				('t '())
			)
		)
		('t
			(cond
				((equal (getValue i j mat) player2) (isSandwichVertical (1- i) j player1 player2 mat n '1))
				((equal (getValue i j mat) player1) i)
				('t '())
			)
		)
	)
)

(defun isSandwichHorizontal (i j player1 player2 mat n direction)
	(cond 
		((= direction 0) (list (isSandwichHorizontal i (1- j) player1 player2 mat n '1) (isSandwichHorizontal i (1+ j) player1 player2 mat n -1)))
		((< direction 0)
			(cond
				((equal (getValue i j mat) player2) (isSandwichHorizontal i (1+ j) player1 player2 mat n -1))
				((equal (getValue i j mat) player1) j)
				('t '())
			)
		)
		('t
			(cond
				((equal (getValue i j mat) player2) (isSandwichHorizontal i (1- j) player1 player2 mat n '1))
				((equal (getValue i j mat) player1) j)
				('t '())
			)
		)
	)
)

(defun removeHorizontalEnemy (i startJ endJ j mat)
	(cond
		((null startJ) (removeHorizontalEnemy i j endJ j mat))
		((null endJ) (removeHorizontalEnemy i startJ j j mat))
		((equal startJ j) (removeHorizontalEnemy i (1+ startJ) endJ j mat)) 
		((< startJ endJ) (removeHorizontalEnemy i (1+ startJ) endJ j (setFigure (list i startJ) '0 mat)))
		('t mat)
	)
)

(defun removeVerticalEnemy (j startI endI i mat)
	(cond 
		((null startI) (removeVerticalEnemy j i endI i mat))
		((null endI) (removeVerticalEnemy j startI i i mat))
		((equal startI i) (removeVerticalEnemy j (1+ startI) endI i mat)) 
		((< startI endI) (removeVerticalEnemy j (1+ startI) endI i (setFigure (list startI j) '0 mat)))
		('t mat)
	)
)


(defun sandwich (i j player1 player2 mat n )
	(let
		(
			(x (isSandwichHorizontal i j player1 player2 mat n 0))
			(y (isSandwichVertical i j player1 player2 mat n 0))
		)
		(cond 
			((and (null (car x)) (null (cadr x)) (null (car y)) (null (cadr y))) mat)
			((and (null (car x)) (null (cadr x))) (removeVerticalEnemy j (ifNullSetPoint (car y)) (cadr y) i mat))
			((and (null (car y)) (null (cadr y))) (removeHorizontalEnemy i (ifNullSetPoint (car x)) (cadr x) j mat))
			('t (removeVerticalEnemy j (ifNullSetPoint (car y)) (cadr y) i (removeHorizontalEnemy i (ifNullSetPoint (car x)) (cadr x) j mat)))
		)
	)
)

(defun ifNullSetPoint (x)
	(cond
		((null x)  x)
		('t (+ x 1))
	)
)

(defun sumVertical (i j player mat n direction)
	(cond 
		((= direction 0) (+ 1 (sumVertical (1+ i) j player mat n -1) (sumVertical (1- i) j player mat n '1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumVertical (1+ i) j player mat n -1) '1))
				('t '0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumVertical (1- i) j player mat n 1) '1))
				('t '0)
			)
		)
	)
)

(defun sumDiagonalRight (i j player mat n direction)
	(cond 
		((= direction 0) (+ 1 (sumDiagonalRight (1+ i) (1+ j) player mat n -1) (sumDiagonalRight (1- i) (1- j) player mat n '1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumDiagonalRight (1+ i) (1+ j) player mat n -1) '1))
				('t '0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i player n)) (+ (sumDiagonalRight (1- i) (1- j) player mat n 1) '1))
				('t '0)
			)
		)
	)
)

(defun sumDiagonalLeft (i j player mat n direction)
	(cond 
		((= direction 0) (+ 1 (sumDiagonalLeft (1+ i) (1- j) player mat n -1) (sumDiagonalLeft (1+ i) (1- j) player mat n '1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumDiagonalLeft (1+ i) (1- j) player mat n -1) '1))
				('t '0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i player n)) (+ (sumDiagonalLeft (1+ i) (1- j) player mat n 1) '1))
				('t '0)
			)
		)
	)
)

(defun isWin (i j player mat n)
	(cond
		((> (sumVertical i j player mat n 0) '4) 't)
		((> (sumDiagonalRight i j player mat n 0) '4) 't)
		((> (sumDiagonalLeft i j player mat n 0) '4) 't)
		('t '())
	)
)

(defun notInOwnField (i player n)
	(cond 
		(
			(= player 1)	
			(cond
				((> i 2) 't)
				('t '())
			)
		)
		(
			(= player 2)
			(cond
				((< i (1- n)) 't)
				('t '())
			)
		)
	)
)

(defun readList (mov)
	(if (not (streamp mov))
		(readList (make-string-input-stream mov))
		(if (listen mov)
			(cons (read mov) (readList mov))
			'()
		)
	)
)