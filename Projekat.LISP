
;ovom funkcijom se pocinje igra, zove funkcije za unos parametara igre - velicina table i ko prvi igra
(defun startGame ()
	(game (startGameInternNRead '()) (startGameInternPRead '()))
)

;ucitava velicinu table od korisnika, err je 't ako unos nije validan
(defun startGameInternNRead (err)
	(progn 
		(cond
			((not (null err)) (format t "~%Pogresno ste uneli velicinu table: "))
			('t (format t "~%Unesite velicinu table: "))
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

;ucitava broj igraca koji je prvi na potezu, err ima istu ulogu kao i u funkciji startGameInternNRead
(defun startGameInternPRead (err)
	(progn 
		(cond
			((not (null err)) (format t "~%Pogresno ste uneli prvog igraca (1 X) (2 O): "))
			('t (format t "~%Unesite prvog igraca (1 X) (2 O): "))
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

;inicijalizuje tablu sa pratecim podacima i pokrece igru
(defun game (n p)
	(gamePlay (wrapMatrix p (* n 2) (* n 2) n (initMatrix 1 n)))
)

;game loop koji u svakom potezu ispituje win condition, da li je doslo do sendvica
;takodje pita korisnika koji je na potezu koji je novi potez i po unosu ga igra
(defun gamePlay (mat)
	(showMatrix (getN mat) mat)
	(let*
		(
			(currP (getP mat))
			(nextP (logxor currP 3))
			(move (promptForTurn currP '() mat))
			(matrix (sandwich (caadr move) (cadadr move) currP nextP (playTurn mat move)))
		)
		(cond
			((isWin (caadr move) (cadadr move) currP matrix (getN matrix)) (write "you win")) 
			('t (gamePlay (setP matrix nextP)))
		)
	)
)

;menja tablu odigravanjem datog poteza tako sto prvo skine figuru sa polaznog polja, a zatim je postavi na odrediste
(defun playTurn(mat turn)
	(setFigure (cadr turn) (getP mat) (unsetFigure (car turn) mat))
)

;dodavanje potrebnih podataka u strukturu koja sadrzi tablu
;pamte se velicina polja, ko je na potezu, broj figura jednog i drugog igraca i da li je igra gotova
;kljuc 64 se koristi za pamcenje ovih podataka u strukturi gde kljucevi inace oznacavaju broj vrste
(defun wrapMatrix(p brX brO n mat)
	(append (list (cons 64 (list (cons n (cons p (cons brX (cons brO (cons '0 '())))))))) mat)
)  

;inicijalizuje tablu, koja se pamti kao asocijativna lista asocijativnih listi
;pamti se u obliku ((<broj vrste> ((<broj kolone> <vrednost polja>) ...)) ...)
(defun initMatrix (i n)
	(cond
		((= i (1+ n)) '())
		((< i 3) (cons (append (list i) (list (initRow 1 n 1))) (initMatrix (1+ i) n)))
		((> i (- n 2)) (cons (append (list i) (list (initRow 1 n 2))) (initMatrix (1+ i) n)))
		('t (cons (append (list i) (list (initRow 1 n 0))) (initMatrix (1+ i) n)))
	)
)

;inicijalizuje jednu vrstu table sa konkretnim elementima (0 - '-', 1 - 'X', 2 - 'O')
(defun initRow (j n el)
	(cond
		((= j (1+ n)) '())
		('t (cons (list j el) (initRow (1+ j) n el)))
	)
)

;stampa sadrzaj table na standardni izlaz
(defun showMatrix (n mat)
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
						((> j 0) (format t "|~a|" (getLetter (getValue i j mat))))
						('t '" ")
					)
				)
			) 
		)
	)
)

;konvertuje vrednost polja u karakter koji treba prikazati (0 -> '-', 1 -> 'X', 2 -> 'O')
(defun getLetter (p)
	(cond
		((= p 0) "-")
		((= p 1) "X")
		('t "O")
	)
)

;konvertuje uneti karakter u broj vrste ('A' -> 1, 'B' -> 2, ..., 'Z' -> 26)
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

;konvertuje uneti broj vrste/kolone u karakter koji treba prikazati (1 -> 'A', 2 -> 'B', ..., 26 -> 'Z')
(defun indToChar (ind)
	(cond 
		((< ind 0) '())
		((= ind 0) '())
		((> ind 26) '())
		('t (car (list (intern (string (code-char (+ ind 64)))))))
	)
)

;vraca velicinu table
(defun getN (mat)
	(caadr (assoc 64 mat))
)

;vraca igraca koji je trenutno na potezu
(defun getP (mat)
	(cadadr (assoc 64 mat))
)

;braca broj figura igraca X
(defun getNX (mat)
	(caddr (cadr (assoc 64 mat)))
)

;vraca broj figura igraca O
(defun getNO (mat)
	(caddr (cdadr (assoc 64 mat)))
)

;vraca da li je igra gotova
(defun getFinished (mat)
	(caddddr (cadr (assoc 64 mat)))
)

;postavlja novog igraca koji je na potezu, prakticno radi wrapMatrix sa novom vrednoscu
(defun setP (mat p)
	(append (list (cons 64 (list (cons (caadr (assoc 64 mat)) (cons p (cddadr (assoc 64 mat))))))) (cdr mat))
)

;postavlja novi broj figura igraca X, slicno kao funkcija setP
(defun setNX (mat newNX)
	(append (list (cons 64 (list (append (list (caadr (assoc 64 mat)) (cadadr (assoc 64 mat)))(cons newNX (cdr (cddadr (assoc 64 mat)))))))) (cdr mat))
)

;postavlja novi brjo figura igraca O, slicno kao funkcije setP i setNX
(defun setNO (mat newNO)
	(append (list (cons 64 (list (append (list (caadr (assoc 64 mat)) (cadadr (assoc 64 mat)) (car (cddadr (assoc 64 mat)))) (cons newNO (cddr (cddadr (assoc 64 mat)))))))) (cdr mat))
)

;dekrementira broj figura igraca p, koristi se prilikom brisanja horizontalnog/vertikalnog niza figura pri pravljenju sendvica
(defun setNewFigureCount (mat p)
	(cond 
		((= p 1) (setNX mat (1- (getNX mat))))
		('t (setNO mat (1- (getNO mat))))
	)
)

;ucitava potez oblika '<polazna vrsta> <polazna kolona> <dolazna vrsta> <dolazna kolona>' od korisnika
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

;nakon ispitivanja validnosti poteza vraca potez strukture ((<polazna vrsta> <polazna kolona>) (<dolazna vrsta> <dolazna kolona>))
(defun makeTurn (turn mat)
	(cond
		((turnInvalid turn (getN mat) (getP mat) mat) '())
		('t (makeTurnStruct turn))
	)
)

;pravi strukturu poteza koju vraca funkcija makeTurn
(defun makeTurnStruct (turn)
	(list 
		(list (charToInd (car turn)) (cadr turn)) 
		(list (charToInd (caddr turn)) (cadddr turn))
	)
)

;ispituje validnost poteza koji je uneti
;ispituje kompletnost unosa, prekoracenje table, validnost pravca kretanja, zauzetost dolaznog polja, odsustvo figure na polaznom polju
;takodje ispituje da li je potez blokiran, tj da li nema figura izmedju polaznog i dolaznog polja
(defun turnInvalid (turn n p mat)
	(let*
		(
			(turnStruct (makeTurnStruct turn))
			(rowF (caar turnStruct))
			(rowT (caadr turnStruct))
			(colF (cadar turnStruct))
			(colT (cadadr turnStruct))
		)
		(cond
			((< (length (car turnStruct)) 2) 't)
			((< (length (cadr turnStruct)) 2) 't)
			((equal (equal colF colT) (equal rowF rowT)) 't)
			((or (> colF n) (< colF 1)) 't)
			((or (> rowF n) (< rowF 1)) 't)
			((or (> colT n) (< colT 1)) 't)
			((or (> rowT n) (< rowT 1)) 't)
			((not (= (getValue rowF colF mat) p)) 't)
			((> (getValue rowT colT mat) 0) 't)
			((turnBlocked turnStruct p mat) 't)
			('t '())
		)
	)
)

;ispituje da li je potez blokiran, tj da li ima figura izmedju polaznog i dolaznog polja poteza
(defun turnBlocked (turnStruct p mat)
	(let*
		((offset (turnOffset turnStruct)) (offsetH (cadr offset)) (offsetV (car offset)))
		(cond
			((not (= offsetH 0)) (horizontalTurnBlocked (signum offsetH) offsetH turnStruct p mat))
			('t (verticalTurnBlocked (signum offsetV) offsetV turnStruct p mat))
		)
	)
)

;ispituje blokiranje poteza po horizontali, sluzi funkciji turnBlocked
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
			('t 	(if (< currMove 0)
					(horizontalTurnBlocked (1- currMove) offset turnStruct p mat)
					(horizontalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
		)
	)
)

;ispituje blokiranje poteza po vertikali, sluzi funkciji turnBlocked
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
			((> (getValue x y mat) 0) 't)
			('t 	(if (< currMove 0)
					(verticalTurnBlocked (1- currMove) offset turnStruct p mat)
					(verticalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
		)
	)
)

;racuna pomeraj poteza koji je potreban funkciji turnBlocked
(defun turnOffset (turnStruct)
	(list (- (caadr turnStruct) (caar turnStruct)) (- (cadadr turnStruct) (cadar turnStruct)))
)

;vraca vrednost polja table sa koordinatama (i, j)
(defun getValue (i j mat)
	(cadr (assoc j (cadr (assoc i mat))))
)

;resetuje vrednost polja table na poziciji pos
(defun unsetFigure (pos mat)
	(cond
		((null mat) '())
		((equal (car pos) (caar mat)) (cons (cons (caar mat) (list (unsetFromRow (cadr pos) (cadar mat)))) (cdr mat)))
		('t (cons (car mat) (unsetFigure pos (cdr mat))))
	)
)

;resetuje polje sa indeksom ind u vrsti row, sluzi funkciji unsetFigure
(defun unsetFromRow (ind row)
	(cond
		((null row) '())
		((equal ind (caar row)) (cons (list ind 0) (cdr row)))
		('t (cons (car row) (unsetFromRow ind (cdr row))))
	)
)

;postavlja vrednost val na polje table sa pozicijom pos
(defun setFigure (pos val mat)
	(cond
		((null mat) '())
		((equal (car pos) (caar mat)) (cons (list (caar mat) (setToRow (cadr pos) val (cadar mat))) (cdr mat)))
		('t (cons (car mat) (setFigure pos val (cdr mat))))
	)
)

;postavlja vrednost val na polje sa indeksom ind u vrsti row, sluzi funkciji setFigure
(defun setToRow (ind val row)
	(cond
		((null row) '())
		((equal ind (caar row)) (cons (list ind val) (cdr row)))
		('t (cons (car row) (setToRow ind val (cdr row))))
	)
)

;ispituje postojanje sendvica po vertikali, vraca koordinate pocetka i koordinate kraja sendvica
;sve izmedju ovih koordinata treba da bude uklonjeno (protivnicke figure)
(defun isSandwichVertical (i j player1 player2 mat direction)
	(cond
		((= direction 0) (list (isSandwichVertical (1- i) j player1 player2 mat '1) (isSandwichVertical (1+ i) j player1 player2 mat -1)))
		((< direction 0)
			(cond
				((equal (getValue i j mat) player2) (isSandwichVertical (1+ i) j player1 player2 mat -1))
				((equal (getValue i j mat) player1) i)
				('t '())
			)
		)
		('t
			(cond
				((equal (getValue i j mat) player2) (isSandwichVertical (1- i) j player1 player2 mat '1))
				((equal (getValue i j mat) player1) i)
				('t '())
			)
		)
	)
)

;ispituje postojanje sendvica po horizontali, vraca koordinate pocetka i koordinate kraja sendvica
;sve izmedju ovih koordinata treba da bude uklonjeno (protivnicke figure)
(defun isSandwichHorizontal (i j player1 player2 mat direction)
	(cond 
		((= direction 0) (list (isSandwichHorizontal i (1- j) player1 player2 mat '1) (isSandwichHorizontal i (1+ j) player1 player2 mat -1)))
		((< direction 0)
			(cond
				((equal (getValue i j mat) player2) (isSandwichHorizontal i (1+ j) player1 player2 mat -1))
				((equal (getValue i j mat) player1) j)
				('t '())
			)
		)
		('t
			(cond
				((equal (getValue i j mat) player2) (isSandwichHorizontal i (1- j) player1 player2 mat '1))
				((equal (getValue i j mat) player1) j)
				('t '())
			)
		)
	)
)

;brise protivnicke figure izmedju pozicija (i, startJ) i (i, endJ), j je kolona igraca koji pravi sendvic
;brise po horizontali, radi na osnovu rezultata funkcije isSandwichHorizontal
(defun removeHorizontalEnemy (i startJ endJ j mat p)
	(cond
		((null startJ) (removeHorizontalEnemy i j endJ j mat p))
		((null endJ) (removeHorizontalEnemy i startJ j j mat p))
		((equal startJ j) (removeHorizontalEnemy i (1+ startJ) endJ j mat p)) 
		((< startJ endJ) (removeHorizontalEnemy i (1+ startJ) endJ j (setFigure (list i startJ) '0 (setNewFigureCount mat p)) p))
		('t mat)
	)
)

;brise protivnicke figure izmedju pozicija (startI, j) i (endI, j), i je vrsta igraca koji pravi sendvic
;brise po vertikali, radi na osnovu rezultata funkcije isSandwichVertical
(defun removeVerticalEnemy (j startI endI i mat p)
	(cond 
		((null startI) (removeVerticalEnemy j i endI i mat p))
		((null endI) (removeVerticalEnemy j startI i i mat p))
		((equal startI i) (removeVerticalEnemy j (1+ startI) endI i mat p)) 
		((< startI endI) (removeVerticalEnemy j (1+ startI) endI i (setFigure (list startI j) '0 (setNewFigureCount mat p)) p))
		('t mat)
	)
)

;testira postojanje sendvica, brise protivnicke figure ukoliko sendvic postoji
;(i, j) su koordinate odigranog poteza
(defun sandwich (i j player1 player2 mat)
	(let
		(
			(x (isSandwichHorizontal i j player1 player2 mat 0))
			(y (isSandwichVertical i j player1 player2 mat 0))
		)
		(cond 
			((and (null (car x)) (null (cadr x)) (null (car y)) (null (cadr y))) mat)
			((and (null (car x)) (null (cadr x))) (removeVerticalEnemy j (ifNullSetPoint (car y)) (cadr y) i mat player2))
			((and (null (car y)) (null (cadr y))) (removeHorizontalEnemy i (ifNullSetPoint (car x)) (cadr x) j mat player2))
			('t (removeVerticalEnemy j (ifNullSetPoint (car y)) (cadr y) i (removeHorizontalEnemy i (ifNullSetPoint (car x)) (cadr x) j mat player2) player2))
		)
	)
)

;ukoliko je vrsta/kolona igraca koji pravi sendvic null, vraca je kao pocetnu koordinatu za brisanje sendvica
;u suprotnom vraca prvu sledecu poziciju
(defun ifNullSetPoint (x)
	(cond
		((null x) x)
		('t (+ x 1))
	)
)

;slicno kao za sendvic, samo sto sabira koliko uzastopnih figura su figure igraca koji je odgigrao potez
;(i, j) su koordinate odigranog poteza, direction oznacava pravac u kom se vrsi vertikalni obilazak
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

;sabira koliko uzastopnih figura su figure igraca koji je odgigrao potez na poziciju (i, j)
;direction oznacava pravac u kom se vrsi obilazak glavne/sporedne dijagonale, u zavisnosti od toga da li je igrac X ili O
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

;sabira koliko uzastopnih figura su figure igraca koji je odgigrao potez na poziciju (i, j)
;direction oznacava pravac u kom se vrsi obilazak sporedne/glavne dijagonale, u zavisnosti od toga da li je igrac X ili O
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

;ispituje win condition, da li protivnik ima manje od 5 figura, ili da li igrac koji je odigrao potez na poziciju (i, j) ima 5 uzastopnih figura
(defun isWin (i j player mat n)
	(cond
		((= player 1) (if (< (getNO mat) '5) 't '()))
		((= player 2) (if (< (getNX mat) '5) 't '()))
		((> (sumVertical i j player mat n 0) '4) 't)
		((> (sumDiagonalRight i j player mat n 0) '4) 't)
		((> (sumDiagonalLeft i j player mat n 0) '4) 't)
		('t '())
	)
)

;vraca 't ukoliko igrac nije u svom polju, koristi se prilikom ispitivanja win conditiona - ne sabiraju se pocetna polja tog igraca
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

;ucitava potez sa standardnog ulaza, konvertuje uneti string u string stream iz koga cita, da bi se dobio simbol 'A' umesto karaktera '#\A'
;dobija se lista oblika (<polazna vrsta : char> <polazna kolona : num> <dolazna vrsta : char> <dolazna kolona : num>)
(defun readList (mov)
	(if (not (streamp mov))
		(readList (make-string-input-stream mov))
		(if (listen mov)
			(cons (read mov) (readList mov))
			'()
		)
	)
)

(defun getMoveList (mat p)
	(let
		((positions (getAllFigurePositions mat p '())))
		(getPossibleMoves mat p positions)
	)
)

(defun getPossibleMoves (mat p positions)
	(cond
		((null positions) '())
		('t (append (getMovesFromPosition mat p (car positions)) (getPossibleMoves mat p (cdr positions))))
	)
)

(defun getAllFigurePositions(mat p li)
	(dotimes (i (1+ n))
		(cond
			((= i 0))
			('t	
				(dotimes (j (1+ n))
					(cond
						((> j 0)  (if (equal (getValue i j mat) p) (cons (list i j) li)))
						('t )
					)
				)
			) 
		)
	)
	li
)

(defun getMovesFromPosition (pot player mat)
	(append (verticalMoves pot pot player mat 0) (horizontalMoves pot pot player mat 0))
)

(defun horizontalMoves (pot prev player mat direction)
	(cond 
		((= direction 0) (append (horizontalMoves (list (car pot) (1- (cadr pot))) prev player  mat 1) (horizontalMoves (list (car pot) (1+ (cadr pot))) prev player mat -1)))
		((< direction 0)
			(cond
				((not (equal (getValue (car pot) (cadr pot) mat) 0)) '())
				('t (cons (list prev pot) (horizontalMoves (list (car pot) (1+ (cadr pot))) prev player  mat -1)))
				
			)
		)
		('t
			(cond
				((not (equal (getValue (car pot) (cadr pot) mat) 0)) '())
				('t (cons (list prev pot) (horizontalMoves (list (car pot) (1- (cadr pot))) prev player mat 1)))
			)
		)
	)
)

(defun verticalMoves (pot prev player  mat direction)
	(cond
		((= direction 0) (append (verticalMoves (list (1- (car pot)) (cadr pot)) prev player mat 1) (verticalMoves (list (1+ (car pot)) (cadr pot)) prev player mat -1)))
		((< direction 0)
			(cond
				((not (equal (getValue (car pot) (cadr pot) mat) 0)) '())
				('t (cons (list prev pot)(verticalMoves (list (1+ (car pot)) (cadr pot)) prev player mat -1)))
			)
		)
		('t
			(cond
				((not (equal (getValue (car pot) (cadr pot) mat) 0)) '())
				('t (cons (list prev pot)(verticalMoves (list (1- (car pot)) (cadr pot)) prev player mat 1)))
			)
		)
	)
)

