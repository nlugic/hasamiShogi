
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
					(car (alphaBeta '() mat currP 3 -2001 2001 (getN mat)))
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
			((and (= (abs offset) 2) (= (getValue x y mat) (logxor p 3)))
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
			((and (= (abs offset) 2) (= (getValue x y mat) (logxor p 3)))
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
	(nth j (nth i (car mat)))
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
		((= direction 0) (+ 1 (sumDiagonalLeft (1+ i) (1- j) player mat n -1) (sumDiagonalLeft (1+ i) (1- j) player mat n 1)))
		((< direction 0)
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i  player n)) (+ (sumDiagonalLeft (1+ i) (1- j) player mat n -1) 1))
				('t 0)
			)
		)
		('t
			(cond
				((and (equal (getValue i j mat) player) (notInOwnField i player n)) (+ (sumDiagonalLeft (1+ i) (1- j) player mat n 1) 1))
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

;osnova alfa-beta algoritma, nalazi moguce poteze za trenutno stanje i zapocinje obradu u zavisnosti od igraca na potezu (min ili max)
;pot sluzi za pozive alphaBeta iz funkcija minState/maxState, on se vraca ako je dubina 0, ili ako nema vise poteza
(defun alphaBeta (pot mat p d alfa beta N)
	(let
		((listaPoteza (getMoveList mat p N)))
		(cond
			((or (null listaPoteza) (zerop d)) (evaluate pot mat p N))
			((= p 1) (maxState mat listapoteza (cons (car listaPoteza) (list -1000)) d alfa beta N))
			('t (minState mat listapoteza (cons (car listaPoteza) (list 1000)) d alfa beta N))
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
					(currPV (list (car listPot ) (cadr (alphaBeta (list (car listPot) (* (- (GetNO mat) (GetNO novaMat)) 60)) novaMat 2 (1- d) alfa beta N))))
				)
				(cond
					((< beta (1- alfa)) maxPV)
					((> (cadr currPV) alfa)  (maxState mat (cdr listPot) currPV d (cadr currPV) beta N))
					((> (cadr currPV) (cadr maxPV)) (maxState mat (cdr listPot) currPV d alfa beta N))
					('t (maxState novaMat (cdr listPot) maxPV d alfa beta N))
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
                                        (currPV (list (car listPot ) (cadr (alphaBeta (list (car listPot) (* (- (GetNX mat) (GetNX novaMat)) 60 -1)) novaMat 1 (1- d) alfa beta N))))
				)
				(cond
					((< beta (1- alfa)) minPV)
					((< (cadr currPV) beta) (minState mat (cdr listPot) currPV d alfa (cadr currPV) N))
					((< (cadr currPV) (cadr minPV)) (minState mat (cdr listPot) currPV d alfa beta N))
					('t (minState novaMat (cdr listPot) minPV d alfa beta N))
                                )
			)
		)
	)
)

(defun getEnemyOrFriend (p)
	(cond
		((= p 1) 1)
		('t -1)
	)
)

;funkcija koja vrsi evaluaciju stanja (poteza), vraca evaluirani potez kao (<potez> <vrednost>), gde je potez oblika ((a b) (c d))
;za sada se vrsi samo ispitivanje da li ima pobednika
(defun evaluate (pot mat p N)
	(let
		((rez (isWin (caaar pot) (cadaar pot) p mat N)))
		(cond
			((and (= p 1) rez) (cons pot (list 2000)))
			((and (= p 2) rez) (cons pot (list -2000)))
			('t (cons (car pot) (lista (+ (cadr pot) (stageTwoEvaluation pot mat p N)))))
		)
	)
)

(defun figuresValue (mat)
	(* 5 (- (getNX mat) (getNO mat)))
)

(defun stageTwoEvaluation(pot mat p N)
	(+ (figuresValue mat) (stageThreeEvaluation pot mat p N))
	;(cond
	;	((= p 1) (+ (getNX mat) (stageThreeEvaluation pot mat p N)))
	;	('t (- (getNO mat) (stageThreeEvaluation pot mat p N)))
	;)
)

(defun stageThreeEvaluation(pot mat p N)
	(cond
		((and (= p 1) (or (= (caadar pot) 0) (= (caadar pot) (- N 1)))) (+ 10 (stageFourEvaluation pot mat p N)))
		((or (= (caadar pot) 0) (= (caadar pot) (- N 1))) (-10 (stageFourEvaluation pot mat p N)))
		('t (stageFourEvaluation pot mat p N))
	)
)


(defun stageFourEvaluation(pot mat p N)
	(let
		(
			(x (caaar pot))
			(y (cadaar pot))
			(x1 (caadar pot))
			(y1 (cadadr (car pot)))
		)
		(cond
			((and (= x x1) (= (abs (- y y1)) 2) (= (getValue x (+ y (/ (- y1 y) 2)) mat) (logxor p 3))) (+ (* 25 (getEnemyOrFriend p)) (stageFiveEvaluation pot mat p N)))
			((and (= y y1) (= (abs (- x x1)) 2) (= (getValue (+ x (/ (- x1 x) 2)) y mat) (logxor p 3))) (+ (* 25 (getEnemyOrFriend p)) (stageFiveEvaluation pot mat p N)))
			('t (stageFiveEvaluation pot mat p N))
		)
	)
)

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

(defun stageFiveEvaluation(pot mat p N)
	(let
		(
			(v (sumVertical (caadar pot) (cadadr(car pot)) p mat N 0))
			(dr (sumDiagonalRight (caadar pot) (cadadr(car pot)) p mat n 0))
			(dl (sumDiagonalLeft (caadar pot) (cadadr(car pot)) p mat n 0))
			(h (sumHorizontal (caadar pot) (cadadr(car pot)) p mat N 0))
			(sv (sandwichNumberHorizontal (caadar pot) (cadadr(car pot)) p (logxor p 3) mat))
			(sh (sandwichNumberVertical (caadar pot) (cadadr(car pot)) p (logxor p 3) mat))
		)
		; (+ (* (+ (* v 100) (* dr 100) (* dl 100) (* sv 250) (* sh 250)) (getEnemyOrFriend p))(* (getEnemyOrFriend p) (* h -75)))
		(+ (* v 100) (* dr 100) (* dl 100) (* sv 250) (* sh 250) (* h -75))  
	)
)

(defun sandwichNumberHorizontal (i j player1 player2 mat)
	(let
		((x (isSandwichHorizontal i j player1 player2 mat 0)))
		(cond 
			((or (null (car x)) (null (cadr x)))  0)
			((and (not (null (car x))) (not (null (cadr x))))  (- (cadr x) (car x) 1))
		)
	)
)

(defun sandwichNumberVertical (i j player1 player2 mat)
	(let
		((y (isSandwichVertical i j player1 player2 mat 0)))
		(cond 
			((or (null (car y)) (null (cadr y))) 0)
			((and (not (null (car y))) (not (null (cadr y))))  (- (cadr y) (car y) 1))
		)
	)
)