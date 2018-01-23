
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
			((P (read)))
			(cond 
				((or (= P 1) (= P 2)) P)
				('t (startGameInternPRead 't))
			)
		)
	)
)

;inicijalizuje tablu sa pratecim podacima i pokrece igru
(defun game (n p)
	(gamePlay (wrapMatrix p (* n 2) (* n 2) (1- n) (initMatrix 0 (1- n))))
)

;game loop koji u svakom potezu ispituje win condition, da li je doslo do sendvica
;takodje pita korisnika koji je na potezu koji je novi potez i po unosu ga igra
;ovde se trenutno ne koristi newState funkcija dodata u 2. fazi jer ona menja igraca koji je na potezu
;a u ovoj funkciji je zamisljeno da se on menja tek na kraju, nakon ispitivanja uslova pobede
(defun gamePlay (mat)
	(showMatrix (getN mat) mat)
	(let*
		(
			(currP (getP mat))
			(nextP (logxor currP 3))
			(move
				(if (= currP 1)
					(promptForTurn currP '() mat)
					(car (alphaBeta '() mat currP 3 -5000 5000 (getN mat)))
					;(let*
					;	(
					;		(allMoves (getMoveList mat currP))
					;		(numOfMoves (length allMoves))
					;	)
					;	(nth (random numOfMoves) allMoves)
					;)
				)
			)
			(matrix (sandwich (caadr move) (cadadr move) currP nextP (playTurn mat move)))
		)
		(cond
			((isWin (caadr move) (cadadr move) currP matrix (getN matrix))
				(showMatrix (getN matrix) matrix)
				(format t "~%~a je pobedio!" (getLetter currP))
			)
			('t (gamePlay (setP matrix nextP)))
		)
	)
)

;menja tablu odigravanjem datog poteza tako sto prvo skine figuru sa polaznog polja, a zatim je postavi na odrediste
(defun playTurn(mat turn)
	(cons (setFigure (cadr turn) (getP mat) (unsetFigure (car turn) (car mat))) (cdr mat))
)

;dodavanje potrebnih podataka u strukturu koja sadrzi tablu
;pamte se velicina polja, ko je na potezu, broj figura jednog i drugog igraca i da li je igra gotova
;kljuc 64 se koristi za pamcenje ovih podataka u strukturi gde kljucevi inace oznacavaju broj vrste
(defun wrapMatrix(p brX brO n mat)
	(cons mat (list (cons n (cons p (cons brX (cons brO '()))))))
)

;inicijalizuje tablu, koja se pamti kao asocijativna lista asocijativnih listi
;pamti se u obliku ((<broj vrste> ((<broj kolone> <vrednost polja>) ...)) ...)
(defun initMatrix (i n)
	(cond
		((> i n) '())
		((< i 2) (cons (initRow 0 n 1) (initMatrix (1+ i) n)))
		((> i (- n 2)) (cons (initRow 0 n 2) (initMatrix (1+ i) n)))
		('t (cons (initRow 0 n 0) (initMatrix (1+ i) n)))
	)
)

;inicijalizuje jednu vrstu table sa konkretnim elementima (0 - '-', 1 - 'X', 2 - 'O')
(defun initRow (j n el)
	(cond
		((> j n) '())
		('t (cons el (initRow (1+ j) n el)))
	)
)

;stampa sadrzaj table na standardni izlaz
(defun showMatrix (n mat)
	(format t "~%")
	(dotimes (g (+ n 2))
		(cond
			((= g 0) (format t "  "))
			('t (format t " ~d " g ))
		)
	)
	(dotimes (i (+ n 2))
		(cond
			((= i 0) (format t "   "))
			('t
				(print (indToChar (1- i)))
				(dotimes (j (+ n 2))
					(cond
						((> j 0) (format t "|~a|" (getLetter (getValue (1- i) (1- j) mat))))
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
			('t (- chCode 65))
		)
	)
)

;konvertuje uneti broj vrste/kolone u karakter koji treba prikazati (1 -> 'A', 2 -> 'B', ..., 26 -> 'Z')
(defun indToChar (ind)
	(cond 
		((< ind 0) '())
		((> ind 26) '())
		('t (car (list (intern (string (code-char (+ ind 65)))))))
	)
)

;vraca velicinu table
(defun getN (mat)
	(nth 0 (nth 1 mat))
)

;vraca igraca koji je trenutno na potezu
(defun getP (mat)
	(nth 1 (nth 1 mat))
)

;braca broj figura igraca X
(defun getNX (mat)
	(nth 2 (nth 1 mat))
)

;vraca broj figura igraca O
(defun getNO (mat)
	(nth 3 (nth 1 mat))
)

(defun setNth (n el lista)
	(cond
		((null lista) '())
		((= n 0) (cons el (cdr lista)))
		('t (cons (car lista) (setNth (1- n) el (cdr lista))))
	)
)

;postavlja novog igraca koji je na potezu, prakticno radi wrapMatrix sa novom vrednoscu
(defun setP (mat p)
	(cons (car mat) (list (setNth 1 p (nth 1 mat))))
)

;postavlja novi broj figura igraca X, slicno kao funkcija setP
(defun setNX (mat newNX)
	(cons (car mat) (list (setNth 2 newNX (nth 1 mat))))
)

;postavlja novi broj figura igraca O, slicno kao funkcije setP i setNX
(defun setNO (mat newNO)
	(cons (car mat) (list (setNth 3 newNO (nth 1 mat))))
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
			('t (format t "~%~a je na potezu: " (getLetter p)))
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
		(list (charToInd (car turn)) (1- (cadr turn)))
		(list (charToInd (caddr turn)) (1- (cadddr turn)))
	)
)

;ispituje validnost poteza koji je unet
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
			((or (> colF n) (< colF 0)) 't)
			((or (> rowF n) (< rowF 0)) 't)
			((or (> colT n) (< colT 0)) 't)
			((or (> rowT n) (< rowT 0)) 't)
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
			((= (- (abs currMove) (abs offset)) 1) '())
			((and (= (abs offset) 2) (not (= (getValue x y mat) 0)))
				(if (< currMove 0)
					(horizontalTurnBlocked (1- currMove) offset turnStruct p mat)
					(horizontalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
			((> (getValue x y mat) 0) t)
			('t 	
				(if (< currMove 0)
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
			((and (= (abs offset) 2) (not (= (getValue x y mat) 0)))
				(if (< currMove 0)
					(verticalTurnBlocked (1- currMove) offset turnStruct p mat)
					(verticalTurnBlocked (1+ currMove) offset turnStruct p mat)
				)
			)
			((> (getValue x y mat) 0) 't)
			('t
				(if (< currMove 0)
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
  (cond ((or (< i 0) (< j 0)) ())
	('t (nth j (nth i (car mat))))
  )
)

;resetuje vrednost polja table na poziciji pos
(defun unsetFigure (pos mat)
	(cond
		((null mat) '())
		((= (car pos) 0) (cons (unsetFromRow (cadr pos) (car mat)) (cdr mat)))
		('t (cons (car mat) (unsetFigure (cons (1- (car pos)) (cdr pos)) (cdr mat))))
	)
)

;resetuje polje sa indeksom ind u vrsti row, sluzi funkciji unsetFigure
(defun unsetFromRow (ind row)
	(cond
		((null row) '())
		((= ind 0) (cons 0 (cdr row)))
		('t (cons (car row) (unsetFromRow (1- ind) (cdr row))))
	)
)

;postavlja vrednost val na polje table sa pozicijom pos
(defun setFigure (pos val mat)
	(cond
		((null mat) '())
		((= (car pos) 0) (cons (setToRow (cadr pos) val (car mat)) (cdr mat)))
		('t (cons (car mat) (setFigure (cons (1- (car pos)) (cdr pos)) val (cdr mat))))
	)
)

;postavlja vrednost val na polje sa indeksom ind u vrsti row, sluzi funkciji setFigure
(defun setToRow (ind val row)
	(cond
		((null row) '())
		((= ind 0) (cons val (cdr row)))
		('t (cons (car row) (setToRow (1- ind) val (cdr row))))
	)
)

;ispituje postojanje sendvica po vertikali, vraca koordinate pocetka i koordinate kraja sendvica
;sve izmedju ovih koordinata treba da bude uklonjeno (protivnicke figure)
(defun isSandwichVertical (i j player1 player2 mat direction)
	(cond
		((or (< i 0) (> i (getN mat))) '())
		((= direction 0) (list (isSandwichVertical (1- i) j player1 player2 mat 1) (isSandwichVertical (1+ i) j player1 player2 mat -1)))
		((< direction 0)
			(cond
				((equal (getValue i j mat) player2) (isSandwichVertical (1+ i) j player1 player2 mat -1))
				((equal (getValue i j mat) player1) i)
				('t '())
			)
		)
		('t
			(cond
				((equal (getValue i j mat) player2) (isSandwichVertical (1- i) j player1 player2 mat 1))
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
		((or (< j 0) (> j (getN mat))) '())
		((= direction 0) (list (isSandwichHorizontal i (1- j) player1 player2 mat 1) (isSandwichHorizontal i (1+ j) player1 player2 mat -1)))
		((< direction 0)
			(cond
				((equal (getValue i j mat) player2) (isSandwichHorizontal i (1+ j) player1 player2 mat -1))
				((equal (getValue i j mat) player1) j)
				('t '())
			)
		)
		('t
			(cond
				((equal (getValue i j mat) player2) (isSandwichHorizontal i (1- j) player1 player2 mat 1))
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
		((< startJ endJ) (removeHorizontalEnemy i (1+ startJ) endJ j (setNewFigureCount (cons (setFigure (list i startJ) '0 (car mat)) (cdr mat)) p) p))
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
		((< startI endI) (removeVerticalEnemy j (1+ startI) endI i (setNewFigureCount (cons (setFigure (list startI j) '0 (car mat)) (cdr mat)) p) p))
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
		((or (< i 0) (< j 0)) 0)
		((= direction 0) (+ 1 (sumVertical (1+ i) j player mat n -1) (sumVertical (1- i) j player mat n 1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumVertical (1+ i) j player mat n -1) 1))
				('t 0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumVertical (1- i) j player mat n 1) 1))
				('t 0)
			)
		)
	)
)

;sabira koliko uzastopnih figura su figure igraca koji je odgigrao potez na poziciju (i, j)
;direction oznacava pravac u kom se vrsi obilazak glavne/sporedne dijagonale, u zavisnosti od toga da li je igrac X ili O
(defun sumDiagonalRight (i j player mat n direction)
	(cond
		((or (< i 0) (< j 0)) 0)
		((= direction 0) (+ 1 (sumDiagonalRight (1+ i) (1+ j) player mat n -1) (sumDiagonalRight (1- i) (1- j) player mat n 1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumDiagonalRight (1+ i) (1+ j) player mat n -1) 1))
				('t 0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i player n)) (+ (sumDiagonalRight (1- i) (1- j) player mat n 1) 1))
				('t 0)
			)
		)
	)
)

;sabira koliko uzastopnih figura su figure igraca koji je odgigrao potez na poziciju (i, j)
;direction oznacava pravac u kom se vrsi obilazak sporedne/glavne dijagonale, u zavisnosti od toga da li je igrac X ili O
(defun sumDiagonalLeft (i j player mat n direction)
	(cond
		((or (< i 0) (< j 0)) 0)
		((= direction 0) (+ 1 (sumDiagonalLeft (1+ i) (1- j) player mat n -1) (sumDiagonalLeft (1- i) (1+ j) player mat n 1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumDiagonalLeft (1+ i) (1- j) player mat n -1) 1))
				('t 0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i player n)) (+ (sumDiagonalLeft (1- i) (1+ j) player mat n 1) 1))
				('t 0)
			)
		)
	)
)

;ispituje win condition, da li protivnik ima manje od 5 figura, ili da li igrac koji je odigrao potez na poziciju (i, j) ima 5 uzastopnih figura
(defun isWin (i j player mat n)
	(cond
		((and (= player 1) (< (getNO mat) 5)) 't)
		((and (= player 2) (< (getNX mat) 5)) 't)
		((> (sumVertical i j player mat n 0) 4) 't)
		((> (sumDiagonalRight i j player mat n 0) 4) 't)
		((> (sumDiagonalLeft i j player mat n 0) 4) 't)
		('t '())
	)
)

;vraca 't ukoliko igrac nije u svom polju, koristi se prilikom ispitivanja win conditiona - ne sabiraju se pocetna polja tog igraca
(defun notInOwnField (i player n)
	(cond 
		(
			(= player 1)
			(cond
				((> i 1) 't)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2. FAZA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NAPOMENA! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;IZVRSENA JE VELIKA PROMENA U STRUKTURI TABLE - TABLA SE SADA PAMTI KAO LISTA LISTI
;KOMPLETNO STANJE IGRE (mat) SADA IMA OBLIK (((kol) (kol) ...) (n p nx no))
;DAKLE (car mat) VRACA LISTU KOLONA TABLE, A (cadr mat) VRACA LISTU ATRIBUTA STANJA
;DVA SU RAZLOGA ZA OVU ODLUKU: JEDNOSTAVNOST KODA I POTENCIJALNO VECE PERFORMANSE
;MNOGO JE LAKSE PRISTUPATI KORISCENJEM NTH NTH NEGO STO JE TO BILO SA ASSOC ASSOC
;A POSTOJE I NAZNAKE DA JE PRISTUP BRZI UZ KORISCENJE FUNKCIJE NTH
;																				   -LUGIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;pomocna funkcija koja uklanja sve null vrednosti iz liste, koristi se za ciscenje liste poteza koju vraca funkcija jumpMoves
(defun removeNulls (lista)
	(cond
		((null lista) '())
		((null (car lista)) (removeNulls (cdr lista)))
		('t (cons (car lista) (removeNulls (cdr lista))))
	)
)

;funkcija koja vraca sve moguce poteze za datu tablu i igraca koji je na potezu
;prvo nalazi pozicije svih figura tog igraca, a zatim nalazi moguce poteze iz njih pozivajuci funkciju getPossibleMoves
(defun getMoveList (mat p N)
	(let
		((positions (getAllFigurePositions mat p 0 N)))
		(getPossibleMoves mat p positions)
	)
)

;za svaku od figura nalazi njene moguce poteze, sve poteze smesta u jednu listu i to je lista svih poteza
(defun getPossibleMoves (mat p positions)
	(cond
		((null positions) '())
		('t (append (getMovesFromPosition mat p (car positions)) (getPossibleMoves mat p (cdr positions))))
	)
)

;za jednu figuru igraca nalazi sve njene poteze, pos je pozicija figure a p igrac koji je na potezu
;svi potezi figure su unija svih preskoka, poteza po vertikali i poteza po horizontali koje figura moze da obavi
(defun getMovesFromPosition (mat p pos)
	(append (jumpMoves pos pos p mat 0 0) (verticalMoves pos pos p mat 0) (horizontalMoves pos pos p mat 0))
)

;nalazi sve poteze preskoka igraca p sa pozicije pos, prev je prethodna pozicija, i i dir su pomocne promenljive
;i je dubina rekurzije funkcije na osnovu koje se vrsi obrada (0 -> pocetak, 1 -> ispituje susednu figuru, 2 -> ispituje polje za doskok)
;dir je pravac u kome se vrsi ispitivanje (0 -> desno, 1 -> levo, 2 -> dole, 3 -> gore)
(defun jumpMoves (pos prev p mat i dir)
	(cond
		((or (< (car pos) 0) (< (cadr pos) 0)) '())
        ((= i 0)
			(removeNulls
				(list
					(jumpMoves (list (car pos) (1+ (cadr pos))) prev p mat 1 0)
					(jumpMoves (list (car pos) (1- (cadr pos))) prev p mat 1 1)
					(jumpMoves (list (1+ (car pos)) (cadr pos)) prev p mat 1 2)
					(jumpMoves (list (1- (car pos)) (cadr pos)) prev p mat 1 3)
				)
			)
		)
		((= i 1)
			(cond
				(
					(not (or (equal (getValue (car pos) (cadr pos) mat) 0) (equal (getValue (car pos) (cadr pos) mat) p)))
					(cond
						((= dir 0) (jumpMoves (list (car pos) (1+ (cadr pos))) prev p mat 2 0))
						((= dir 1) (jumpMoves (list (car pos) (1- (cadr pos))) prev p mat 2 1))
						((= dir 2) (jumpMoves (list (1+ (car pos)) (cadr pos)) prev p mat 2 2))
						((= dir 3) (jumpMoves (list (1- (car pos)) (cadr pos)) prev p mat 2 3))
                    )
				)
			)
		)
		((= i 2)
			(cond
				((equal (getValue (car pos) (cadr pos) mat) 0) (list prev pos))
				('t '())
			)
		)
	)
)

;nalazi sve horizontalne poteze igraca p sa pozicije pos, prev je prethodna pozicija
;direction je pomocna promenljiva koja oznacava smer kretanja (1 -> desno, -1 -> levo)
(defun horizontalMoves (pos prev p mat direction)
	(cond
		((or (< (car pos) 0) (< (cadr pos) 0)) '())
		((= direction 0)
			(append
				(horizontalMoves (list (car pos) (1- (cadr pos))) prev p mat 1)
				(horizontalMoves (list (car pos) (1+ (cadr pos))) prev p mat -1)
			)
		)
		((< direction 0)
			(cond
				((not (equal (getValue (car pos) (cadr pos) mat) 0)) '())
				('t (cons (list prev pos) (horizontalMoves (list (car pos) (1+ (cadr pos))) prev p  mat -1)))
			)
		)
		('t
			(cond
				((not (equal (getValue (car pos) (cadr pos) mat) 0)) '())
				('t (cons (list prev pos) (horizontalMoves (list (car pos) (1- (cadr pos))) prev p mat 1)))
			)
		)
	)
)

;nalazi sve vertikalne poteze igraca p sa pozicije pos, prev je prethodna pozicija
;direction je pomocna promenljiva koja oznacava smer kretanja (1 -> nanize, -1 -> navise)
(defun verticalMoves (pos prev p mat direction)
	(cond
		((or (< (car pos) 0) (< (cadr pos) 0)) '())
		((= direction 0)
			(append
				(verticalMoves (list (1- (car pos)) (cadr pos)) prev p mat 1)
				(verticalMoves (list (1+ (car pos)) (cadr pos)) prev p mat -1)
			)
		)
		((< direction 0)
			(cond
				((not (equal (getValue (car pos) (cadr pos) mat) 0)) '())
				('t (cons (list prev pos) (verticalMoves (list (1+ (car pos)) (cadr pos)) prev p mat -1)))
			)
		)
		('t
			(cond
				((not (equal (getValue (car pos) (cadr pos) mat) 0)) '())
				('t (cons (list prev pos) (verticalMoves (list (1- (car pos)) (cadr pos)) prev p mat 1)))
			)
		)
	)
)

;funkcija koja nalazi pozicije svih figura igraca p, i je pomocni indeks koji ide od 0
(defun getAllFigurePositions (mat p i N)
	(cond
		((< N i) '())
		('t (append (getAllFigurePositions mat p (1+ i) N) (getAllFigurePositionsV mat (nth i (car mat)) p i 0 N)))
	)
)

;pomocna funkcija za nalazenje pozicija svih figura igraca p, obradjuje jednu vrstu
(defun getAllFigurePositionsV (mat col p i j N)
	(cond
		((< N j) '())
		((equal (getValue i j mat) p) (cons (list i j) (getAllFigurePositionsV mat col p i (1+ j) N)))
		('t (getAllFigurePositionsV mat col p i (1+ j) N))
	)
)

;menja stanje prosledjenim potezom, vraca stanje posle odigranog poteza
(defun newState (mat move)
	(let*
		((currP (getP mat)) (nextP (logxor currP 3)))
		(setP (sandwich (caadr move) (cadadr move) currP nextP (playTurn mat move)) nextP)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 3. FAZA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;osnova alfa-beta algoritma, nalazi moguce poteze za trenutno stanje i zapocinje obradu u zavisnosti od igraca na potezu (min ili max)
;pot sluzi za pozive alphaBeta iz funkcija minState/maxState, on se vraca ako je dubina 0, ili ako nema vise poteza
(defun alphaBeta (pot mat p d alfa beta N)
	(let
		((listaPoteza (getMoveList mat p N)))
		(cond
			((or (null listaPoteza) (zerop d)) (evaluateState pot mat p N))
			((= p 1) (maxState mat listapoteza (cons (car listaPoteza) (list -5000)) d alfa beta N))
			('t (minState mat listapoteza (cons (car listaPoteza) (list 5000)) d alfa beta N))
		)
	)
)

;funkcija maksimizujuceg igraca (to je u nasem slucaju 1/X), kreira novo stanje na osnovu poteza i ponovo zove alphaBeta nad njim
;za odredjeni nivo stabla predstavljen listom poteza listPot nalazi cvor sa maksimalnom isplativoscu maxPV
;ispituje ima li odsecanja, ima li nove vrednosti alfa, kao i ima li nove maksimalne vrednosti
(defun maxState (mat listPot maxPV d alfa beta N)
	(cond
		((null listPot) maxPV)
		('t
			(let*
				(
					(novaMat (newState mat (car listpot)))
                                        (novoP (getP novaMat))
					(currPV (list (car listPot) (cadr (alphaBeta (list (car listPot) 0) novaMat novoP (1- d) alfa beta N))))
				)
				(cond
					((<= beta alfa) maxPV)
					((> (cadr currPV) alfa)  (maxState mat (cdr listPot) currPV d (cadr currPV) beta N))
					((> (cadr currPV) (cadr maxPV)) (maxState mat (cdr listPot) currPV d alfa beta N))
					('t (maxState mat (cdr listPot) maxPV d alfa beta N))
				)
			)
		)
	)
)

;funkcija minimizujuceg igraca (to je u nasem slucaju 2/O), kreira novo stanje na osnovu poteza i ponovo zove alphaBeta nad njim
;za odredjeni nivo stabla predstavljen listom poteza listPot nalazi cvor sa minimalnom isplativoscu minPV
;ispituje ima li odsecanja, ima li nove vrednosti beta, kao i ima li nove minimalne vrednosti
(defun minState (mat listPot minPV d alfa beta N)
	(cond
		((null listPot) minPV)
		('t
			(let*
				(
					(novaMat (newState mat (car listpot)))
                                        (novoP (getP novaMat))
					(currPV (list (car listPot) (cadr (alphaBeta (list (car listPot) 0) novaMat novoP (1- d) alfa beta N))))
				)
				(cond
					((<= beta alfa) minPV)
					((< (cadr currPV) beta) (minState mat (cdr listPot) currPV d alfa (cadr currPV) N))
					((< (cadr currPV) (cadr minPV)) (minState mat (cdr listPot) currPV d alfa beta N))
					('t (minState mat (cdr listPot) minPV d alfa beta N))
				)
			)
		)
	)
)

;funkcija koja vrsi evaluaciju stanja (poteza), vraca evaluirani potez kao (<potez> <vrednost>), gde je potez oblika ((a b) (c d))
;za sada se vrsi samo ispitivanje da li ima pobednika
(defun evaluateState (pot mat p N)
	(let
		((rez (isWin (caaar pot) (cadaar pot) p mat N)))
		(cond
			((and (= p 1) rez) (cons pot (list 3000)))
			((and (= p 2) rez) (cons pot (list -3000)))
			('t (cons (car pot) (list (stateEval mat N))))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 4. FAZA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                                                ;;;;;
;;;;;                                       INFERENCE ENGINE                                         ;;;;;
;;;;;                                                                                                ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POMOCNE FUNKCIJE

;; provera da li je parametar s izvorna promenljiva (simbol koji pocinje sa ?)
(defun true-var? (s) 
  (if (symbolp s)
      (equal #\? (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s promenljiva (simbol koji pocinje sa ? ili %)
(defun var? (s) 
  (if (symbolp s)
      (let ((c (char (symbol-name s) 0)))
        (or (equal c #\?) (equal c #\%)))
    nil))

;; provera da li je parametar s funkcija (simbol koji pocinje sa =)
(defun func? (s) 
  (if (symbolp s)
      (equal #\= (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s predefinisani predikat (simbol koji pocinje sa !)
(defun predefined-predicate? (s)
  (if (symbolp s)
      (equal #\! (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s konstanta (ako nije promenljiva ili funkcija onda je konstanta)
(defun const? (s)
  (not (or (var? s) (func? s))))

;; rekurzivna provera da li je parametar f funkcija od parametra x
(defun func-of (f x)
  (cond
   ((null f) ; kraj rekurzije
    t)
   ((atom f)
    (equal f x))
   (t
    (or (func-of (car f) x) (func-of (cdr f) x)))))

;; provera da li funkcija f ima promenljivih
(defun has-var (f)
  (cond
   ((null f) 
    nil)
   ((atom f)
    (var? f))
   (t
    (or (has-var (car f)) (has-var (cdr f))))))

;; funkcija koja vraca konsekvencu pravila
(defun rule-consequence (r)
  (car (last r)))

;; funkcija koja vraca premisu pravila
(defun rule-premises (r)
  (let ((p (cadr r)))
    (if (and (listp p) (equal (car p) 'and))
        (cdr p)
      (list p))))
      
;; funkcija koja vrsi prebacivanje upita u interni format (izbacuje 'and)
(defun format-query (q)
  (if (and (listp q) (equal (car q) 'and))
      (cdr q)
    (list q)))
    
;; izracunavanje istinitosne vrednosti predefinisanog predikata
(defun evaluate-predicate (p ls)
  (if (has-var p) nil  ; ako poseduje slobodne promenljive vraca nil (nije validna situacija)
    (if (eval p) 
        (list ls) ; ako predikat vazi vraca ulaznu listu smena
      nil))) ; u suprotnom vraca nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFEJSNE FUNKCIJE I GLOBALNE PROMENLJIVE

(defparameter *FACTS* nil)
(defparameter *RULES* nil)
(defparameter *MAXDEPTH* 10)

;; priprema *FACTS*, *RULES* i *MAXDEPTH*
(defun prepare-knowledge (lr lf maxdepth)
  (setq *FACTS* lf *RULES* (fix-rules lr) *MAXDEPTH* maxdepth))

;; vraca broj rezulata izvodjenja
(defun count-results (q)
  (length (infer- (format-query q) '(nil) 0)))

;; vraca listu lista smena
(defun infer (q)
  (filter-results (infer- (format-query q) '(nil) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE DODELU NOVIH JEDINSTVENIH PROMENLJIVIH PRAVILIMA

(defun fix-rules (lr)
  (if (null lr) nil
    (cons (fix-rule (car lr)) (fix-rules (cdr lr)))))

(defun fix-rule (r)
  (let ((ls (make-rule-ls r nil)))
    (apply-ls r ls)))

(defun make-rule-ls (r ls)
  (cond
   ((null r)
    ls)
   ((var? r)
    (let ((a (assoc r ls)))
      (if (null a)
          (cons (list r (gensym "%")) ls)
        ls)))
   ((atom r)
    ls)   
   (t
    (make-rule-ls (cdr r) 
                  (make-rule-ls (car r) ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE PRIPREMU REZULTATA (IZBACUJU SMENE KOJE SE ODNOSE NA INTERNE PROMENLJIVE)

(defun filter-results (lls)
  (if (null lls) nil
    (cons (filter-result (car lls)) (filter-results (cdr lls)))))

(defun filter-result (ls)
  (if (null ls) nil
    (if (true-var? (caar ls))
        (cons (car ls) (filter-result (cdr ls)))
      (filter-result (cdr ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE SE KORISTE U IZVODJENJU

;; glavna funkcija za izvodjenje, vraca listu lista smena
;; lq - predikati upita
;; lls - lista listi smena (inicijalno lista koja sadrzi nil)
;; depth - tekuca dubina (inicijalno 0)
(defun infer- (lq lls depth)
  (if (null lq) lls
    (let ((lls-n (infer-q (car lq) lls depth)))
      (if (null lls-n) nil
        (infer- (cdr lq) lls-n depth)))))

;; izvodjenje za jedan predikat iz upita, vraca listu lista smena
(defun infer-q (q lls depth)
  (if (null lls) nil
    (let ((lls-n (infer-q-ls q (car lls) depth)))
      (if (null lls-n)
          (infer-q q (cdr lls) depth)
        (append lls-n (infer-q q (cdr lls) depth))))))

;; izvodjenje za jedan predikat sa jednom listom smena, vraca listu lista smena
(defun infer-q-ls (q ls depth)
  (if (predefined-predicate? (car q))
      (evaluate-predicate (apply-ls q ls) ls)
    (if (< depth *MAXDEPTH*)
        (append (infer-q-ls-lf q *FACTS* ls) (infer-q-ls-lr q *RULES* ls depth))
      (infer-q-ls-lf q *FACTS* ls))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; izvodjenje nad bazom cinjenica lf, vraca listu lista smena
(defun infer-q-ls-lf (q lf ls)
  (if (null lf) nil
    (let ((ls-n (infer-q-ls-f q (car lf) ls)))
      (if (null ls-n)
          (infer-q-ls-lf q (cdr lf) ls)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lf q (cdr lf) ls)))))))

;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f (q f ls)
  (if (= (length q) (length f)) ; provera na istu duzinu
      (infer-q-ls-f- q f ls)
    nil))

;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f- (q f ls)
  (if (null q) (list ls)
    (let ((nq (apply-and-eval (car q) ls)) (nf (car f)))
      (if (var? nq) 
          (infer-q-ls-f- (cdr q) (cdr f) (append ls (list (list nq nf))))
        (if (equal nq nf) 
            (infer-q-ls-f- (cdr q) (cdr f) ls)
          nil)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; izvodjenje nad bazom pravila, vraca listu lista smena
(defun infer-q-ls-lr (q lr ls depth)
  (if (null lr) nil
    (let ((ls-n (infer-q-ls-r q (car lr) ls depth)))
      (if (null ls-n)
          (infer-q-ls-lr q (cdr lr) ls depth)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lr q (cdr lr) ls depth)))))))

;; izvodjenje sa jednim pravilom, vraca listu sa listom smena
(defun infer-q-ls-r (q r ls depth)
  (let ((c (rule-consequence r)))
    (if (= (length q) (length c))
        (let ((lsc (unify q c nil ls)))
          (if (null lsc) nil
            (infer- (apply-ls (rule-premises r) (car lsc)) (cdr lsc) (1+ depth))))
      nil)))

;; unifikacija predikata upita q i konsekvence pravila c primenom liste smena ls, vraca listu smena
(defun unify (q c uls ls)
  (if (or (null q) (null c))
      (if (and (null q) (null c)) (list uls ls) nil)
    (let ((eq (car q)) (ec (car c)))
      (cond
       ((equal eq ec)
        (unify (cdr q) (cdr c) uls ls))
       ((var? eq)
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((a (assoc eq ls)))
            (cond
             ((null a)
              (unify (cdr q) (cdr c) uls (cons (list eq ec) ls)))
             ((equal (cadr a) ec)
              (unify (cdr q) (cdr c) uls ls))
             (t 
              nil))))))
       ((func? eq)
        (cond
         ((var? ec)
          (if (func-of eq ec) nil
            (let ((a (assoc ec uls)))
              (cond
               ((null a)              
                (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
               ((equal (cadr a) eq)
                (unify (cdr q) (cdr c) uls ls))
               (t
                nil)))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((f (apply-ls eq ls)))
            (if (has-var f) nil
              (if (equal (eval f) ec)
                  (unify (cdr q) (cdr c) uls ls)
                nil))))))
       (t ;; const
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         (t ;; func or const
          nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMENA LISTE SMENA I IZRACUNAVANJE IZRAZA

(defun apply-and-eval (x ls)
  (if (var? x)
      (apply-ls x ls)
    (if (and (listp x) (func? (car x)))
        (eval (apply-ls x ls)) 
      x)))

;; primena liste smena ls na izraz x
(defun apply-ls (x ls)
  (cond
   ((null x)
    x)
   ((var? x)
    (let ((ax (assoc x ls)))
      (if (null ax) x
        (cadr ax))))
   ((atom x)
    x)
   (t
    (cons (apply-ls (car x) ls) (apply-ls (cdr x) ls)))))

;;;

;vraca broj uzastopnih figura igraca p po horizontali
(defun sumHorizontal (i j p mat n direction)
	(cond
		((or (< i 0) (< j 0)) 0)
		((= direction 0) (+ 1 (sumHorizontal i (1+ j) p mat n -1) (sumHorizontal i (1- j) p mat n 1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) p) (notInOwnField i p n)) (+ (sumHorizontal i (1+ j) p mat n -1) 1))
				('t 0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) p) (notInOwnField i p n)) (+ (sumHorizontal i (1- j) p mat n 1) 1))
				('t 0)
			)
		)
	)
)

;ispituje postojanje pretnje po polje (i, j) tako sto ide u svim pravcima i trazi protivnicku figuru
(defun threatAnalyze (mat i j p hPom vPom jumpCheck)
	(cond
		((and (zerop hPom) (zerop vPom))
			(cond
				((threatAnalyze mat i j p -1 0 '()) 't)
				((threatAnalyze mat i j p 1 0 '()) 't)
				((threatAnalyze mat i j p 0 -1 '()) 't)
				((threatAnalyze mat i j p 0 1 '()) 't)
			)
		)
		('t
			(let*
				(
					(currI (+ i hPom))
					(currJ (+ j vPom))
					(currValue (getValue currI currJ mat))
					(hPomNext (+ hPom (signum hPom)))
					(vPomNext (+ vPom (signum vPom)))
				)
				(cond
					((null currValue) '())
					((= currValue p) 't)
					((and (zerop currValue) (not jumpCheck)) (threatAnalyze mat i j p hPomNext vPomNext '()))
					((and (= (abs hPom) 1) (= currValue (logxor p 3))) (threatAnalyze mat i j p hPomNext vPomNext 't))
					((and (= (abs vPom) 1) (= currValue (logxor p 3))) (threatAnalyze mat i j p hPomNext vPomNext 't))
					('t '())
				)
			)
		)
	)
)

;formira listu cinjenica koje se koriste u zakljucivanju
(defun formirajCinjenice (mat i N)
	(cond
		((< N i) '())
		('t (append (cinjeniceVrste mat i 0 N) (formirajCinjenice mat (1+ i) N)))
	)
)

;izdvaja cinjenice za svako polje i formira konacni niz cinjenica
;gleda se pripadnost poziciji, ko gadja koju poziciju, kao i postojanje niza odredjenog igraca
(defun cinjeniceVrste (mat i j N)
	(cond
		((< N j) '())
		('t
			(let
				((currValue (getValue i j mat)))
				(cond
					((null currValue) '())
					((= currValue 1) (cons (cons 'polje (cons ''1 (cons i (list j)))) (cinjeniceVrste mat i (1+ j) N)))
					((= currValue 2) (cons (cons 'polje (cons ''2 (cons i (list j)))) (cinjeniceVrste mat i (1+ j) N)))
					('t
						(let*
							(
								(p1Gadja (if (threatAnalyze mat i j 1 0 0 '()) (list (cons 'gadja (cons ''1 (cons i (list j))))) '()))
								(p2Gadja (if (threatAnalyze mat i j 2 0 0 '()) (list (cons 'gadja (cons ''2 (cons i (list j))))) '()))
								
								(p1VertNiz (if (and (> (sumVertical i j 1 mat N 0) 2) (> i 1) (< i (1- N))) 
									(list (cons 'vNiz (cons ''1 (cons i (list j))))) '()))
								(p1LDiagNiz (if (and (> (sumDiagonalLeft i j 1 mat N 0) 2) (> i 1) (< i (1- N)))
									(list (cons 'lDNiz (cons ''1 (cons i (list j))))) '()))
								(p1RDiagNiz (if (and (> (sumDiagonalRight i j 1 mat N 0) 2) (> i 1) (< i (1- N)))
									(list (cons 'rDNiz (cons ''1 (cons i (list j))))) '()))
								(p1HorNiz (if (and (> (sumHorizontal i j 1 mat N 0) 2) (> i 1) (< i (1- N)))
									(list (cons 'hNiz (cons ''1 (cons i (list j))))) '()))
								
								(p2VertNiz (if (and (> (sumVertical i j 2 mat N 0) 2) (> i 1) (< i (1- N))) 
									(list (cons 'vNiz (cons ''2 (cons i (list j))))) '()))
								(p2LDiagNiz (if (and (> (sumDiagonalLeft i j 2 mat N 0) 2) (> i 1) (< i (1- N)))
									(list (cons 'lDNiz (cons ''2 (cons i (list j))))) '()))
								(p2RDiagNiz (if (and (> (sumDiagonalRight i j 2 mat N 0) 2) (> i 1) (< i (1- N))) 
									(list (cons 'rDNiz (cons ''2 (cons i (list j))))) '()))
								(p2HorNiz (if (and (> (sumHorizontal i j 2 mat N 0) 2) (> i 1) (< i (1- N)))
									(list (cons 'hNiz (cons ''2 (cons i (list j))))) '()))
							)
							(append
								p1Gadja p2Gadja p1VertNiz p1LDiagNiz p1RDiagNiz p1HorNiz p2VertNiz p2LDiagNiz p2RDiagNiz p2HorNiz
								(cinjeniceVrste mat i (1+ j) N)
							)
						)
					)
				)
			)
		)
	)
)

;pomocne funkcije za definisanje pravila, nasiroko poznate
(defun !eq (x y) (equal x y))
(defun !ne (x y) (not (equal x y)))

(defun =dec (br) (1- br))
(defun =inc (br) (1+ br))

;definicije pravila za zakljucivanje
;razlikujemo nove nizove igraca 1/2, blokove koje pravi igrac 1/2, samim tim i prilike za sendvic
;razlikujemo i prilike za niz, odnosno produzenje niza na vise od 2 figure
(defparameter *HS-RULES*
	'(
		(if (and (polje ?x ?i ?j)(gadja ?x (=inc ?i) ?j)(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=dec ?i) ?j)(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x ?i (=inc ?j))(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x ?i (=dec ?j))(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=inc ?i) (=inc ?j))(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=inc ?i) (=dec ?j))(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=dec ?i) (=inc ?j))(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=dec ?i) (=dec ?j))(!eq ?x '1)) then (p1NoviNiz ?x ?i ?j))
	
		(if (and (polje ?x ?i ?j)(gadja ?x (=inc ?i) ?j)(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=dec ?i) ?j)(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x ?i (=inc ?j))(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x ?i (=dec ?j))(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=inc ?i) (=inc ?j))(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=inc ?i) (=dec ?j))(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=dec ?i) (=inc ?j))(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
		(if (and (polje ?x ?i ?j)(gadja ?x (=dec ?i) (=dec ?j))(!eq ?x '2)) then (p2NoviNiz ?x ?i ?j))
	
		(if (and (vNiz ?x ?i ?j)(!eq ?x '2)(gadja ?y ?i ?j)(!eq ?y '1)) then (p1BlokVNiza ?x ?i ?j))
		(if (and (hNiz ?x ?i ?j)(!eq ?x '2)(gadja ?y ?i ?j)(!eq ?y '1)) then (p1BlokHNiza ?x ?i ?j))
		(if (and (lDNiz ?x ?i ?j)(!eq ?x '2)(gadja ?y ?i ?j)(!eq ?y '1)) then (p1BlokLDNiza ?x ?i ?j))
		(if (and (rDNiz ?x ?i ?j)(!eq ?x '2)(gadja ?y ?i ?j)(!eq ?y '1)) then (p1BlokRDNiza ?x ?i ?j))
		
		(if (and (vNiz ?x ?i ?j)(!eq ?x '1)(gadja ?y ?i ?j)(!eq ?y '2)) then (p2BlokVNiza ?x ?i ?j))
		(if (and (hNiz ?x ?i ?j)(!eq ?x '1)(gadja ?y ?i ?j)(!eq ?y '2)) then (p2BlokHNiza ?x ?i ?j))
		(if (and (lDNiz ?x ?i ?j)(!eq ?x '1)(gadja ?y ?i ?j)(!eq ?y '2)) then (p2BlokLDNiza ?x ?i ?j))
		(if (and (rDNiz ?x ?i ?j)(!eq ?x '1)(gadja ?y ?i ?j)(!eq ?y '2)) then (p2BlokRDNiza ?x ?i ?j))
		
		(if (p1BlokVNiza ?x ?i ?j) then (p1PrilikaZaVSendvic ?x ?i ?j))
		(if (p1BlokHNiza ?x ?i ?j) then (p1PrilikaZaHSendvic ?x ?i ?j))
		
		(if (p2BlokVNiza ?x ?i ?j) then (p2PrilikaZaVSendvic ?x ?i ?j))
		(if (p2BlokHNiza ?x ?i ?j) then (p2PrilikaZaHSendvic ?x ?i ?j))
		
		(if (and (vNiz ?x ?i ?j)(gadja ?x ?i ?j)(!eq ?x '1)) then (p1PrilikaZaVNiz ?x ?i ?j))
		(if (and (lDNiz ?x ?i ?j)(gadja ?x ?i ?j)(!eq ?x '1)) then (p1PrilikaZaLDNiz ?x ?i ?j))
		(if (and (rDNiz ?x ?i ?j)(gadja ?x ?i ?j)(!eq ?x '1)) then (p1PrilikaZaRDNiz ?x ?i ?j))
		
		(if (and (vNiz ?x ?i ?j)(gadja ?x ?i ?j)(!eq ?x '2)) then (p2PrilikaZaVNiz ?x ?i ?j))
		(if (and (lDNiz ?x ?i ?j)(gadja ?x ?i ?j)(!eq ?x '2)) then (p2PrilikaZaLDNiz ?x ?i ?j))
		(if (and (rDNiz ?x ?i ?j)(gadja ?x ?i ?j)(!eq ?x '2)) then (p2PrilikaZaRDNiz ?x ?i ?j))
	)
)

;evaluacija stanja koja koristi masinu za zakljucivanje
;vrsi se poziciona evaluacija, tj. daju se poeni za svaku figuru vise od protivnika
;racuna se broj prilika za sendvic/niz igraca, kao i broj novih nizova za oba igraca
;sve se sabira, pritom mnozimo odgovarajucim koeficijentima
;na kraju se vraca vrednost koja je veca po abs vrednosti
(defun stateEval (mat N)
	(let*
		(
			(NX (getNX mat))
			(NO (getNO mat))
			(pozEval (* (- NX NO) 50))
			(cinjenice (formirajCinjenice mat 0 N))
			(p1Prilike
				(+
					(count-results '(p1PrilikaZaVSendvic ?x ?i ?j))
					(count-results '(p1PrilikaZaHSendvic ?x ?i ?j))
					(count-results '(p1PrilikaZaVNiz ?x ?i ?j))
					(count-results '(p1PrilikaZaLDNiz ?x ?i ?j))
					(count-results '(p1PrilikaZaRDNiz ?x ?i ?j))
				)
			)
			(p1NoviNizovi (count-results '(p1NoviNiz ?x ?i ?j)))
			(p1Blokovi
				(+
					(count-results '(p2BlokVNiza ?x ?i ?j))
					(count-results '(p2BlokHNiza ?x ?i ?j))
					(count-results '(p2BlokLDNiza ?x ?i ?j))
					(count-results '(p2BlokRDNiza ?x ?i ?j))
				)
			)
			(p2Prilike
				(+
					(count-results '(p2PrilikaZaVSendvic ?x ?i ?j))
					(count-results '(p2PrilikaZaHSendvic ?x ?i ?j))
					(count-results '(p2PrilikaZaVNiz ?x ?i ?j))
					(count-results '(p2PrilikaZaLDNiz ?x ?i ?j))
					(count-results '(p2PrilikaZaRDNiz ?x ?i ?j))
				)
			)
			(p2NoviNizovi (count-results '(p2NoviNiz ?x ?i ?j)))
			(p2Blokovi
				(+
					(count-results '(p1BlokVNiza ?x ?i ?j))
					(count-results '(p1BlokHNiza ?x ?i ?j))
					(count-results '(p1BlokLDNiza ?x ?i ?j))
					(count-results '(p1BlokRDNiza ?x ?i ?j))
				)
			)
			
			(p1Poeni (+ pozEval (* p1Prilike 100) (* p1NoviNizovi 105) (* p1Blokovi -120)))
			(p2Poeni (+ pozEval (* p2Prilike -100) (* p2NoviNizovi -105) (* p2Blokovi 120)))
		)
		(prepare-knowledge *HS-RULES* cinjenice 5)
		(if (< p1Poeni (abs p2Poeni)) p2Poeni p1Poeni)
	)
)
