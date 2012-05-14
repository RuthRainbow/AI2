(deftemplate line
    "Winning line (3 in a row)"
    (slot sq1(type INTEGER))
    (slot sq2(type INTEGER))
    (slot sq3(type INTEGER))
    )

(assert (line (sq1 1) (sq2 2) (sq3 3)))
(assert (line (sq1 4) (sq2 5) (sq3 6)))
(assert (line (sq1 7) (sq2 8) (sq3 9)))
(assert (line (sq1 1) (sq2 4) (sq3 7)))
(assert (line (sq1 2) (sq2 5) (sq3 8)))
(assert (line (sq1 3) (sq2 6) (sq3 9)))
(assert (line (sq1 1) (sq2 5) (sq3 9)))
(assert (line (sq1 3) (sq2 5) (sq3 7)))

(deftemplate occupied
    "Square is occupied by player"
    (slot square(type INTEGER))
    (slot player(type STRING))
    )

(deffacts initial-phase
    (phase choose-player))

(defrule player-select
    (phase choose-player)
    =>
    (printout t "Do you want the be X or O? ")
    (assert (player-select (read)))
    )

(defrule valid-choice
    ?phase <- (phase choose-player)
    ?choice <- (player-select ?player&:(or (eq ?player X) (eq ?player O)))
    =>
    (if (eq ?player X) then
        (printout t "choice was valid - h's go first" crlf)
        (assert (phase human-move))
     else
    	(printout t "choice was valid - c's go first" crlf)
        (assert (phase computer-move)))
    ;(assert (player-move ?choice))
    (retract ?phase ?choice)
    (print-board())
    )

(defrule invalid-choice
    ?phase <- (phase choose-player)
    ?choice <- (player-select ?player&~X&~O)
    =>
    (retract ?phase ?choice)
    (assert (phase choose-player))
    (printout t "Please choose X or O." crlf))

(deffunction next-player(?curr)
    (if (eq ?curr O) then
        (printout t "X's go now" crlf)
        (return X)
     else
        (printout t "O's go now" crlf)
        (return O)))

(defrule human-turn
    (phase human-move)
    =>
    (printout t "please make a move by choosing a square between 1 and 9" crlf)
    (assert (human-move (read)))
    )

(defrule valid-move
    ?phase <- (phase human-move)
    ?move <- (human-move ?hmove&:(or (eq ?hmove 1) (eq ?hmove 2) (eq ?hmove 3) (eq ?hmove 4) (eq ?hmove 5) (eq ?hmove 6) (eq ?hmove 7) (eq ?hmove 8) (eq ?hmove 9)))
    =>
    (retract ?move ?phase)
    (bind ?who <- player-move)
    (assert (occupied (square ?move)(player ?who)))
    (assert (phase computer-move))
    ;(modify (player-move next-player()))
    (print-board()))

/*(defrule square-already-occupied
    ?phase <- (phase human-move)
    ?move <- (human-move ?hmove&:(occupied(square (?hmove))))
    =>
    (retract ?move ?phase)
    (assert (phase human-move))
    (printout t "Square already occupied :( ." crlf))*/
    
(defrule invalid-move
    ?phase <- (phase human-move)
    ?move <- (human-move ?hmove&~1&~2&~3&~4&~5&~6&~7&~8&~9)
    ;also need to check square is not already occupied here
    =>
    (retract ?move ?phase)
    (assert (phase human-move))
    (printout t "Please choose a number between 1 and 9." crlf))

(defquery search-coordinate
    "query to find out all the points that X/O occupies:"
    (declare (variables ?pl))
    (occupied (square ?sq) (player ?pl))
    )

(deffunction print-board()
    (bind ?i 1)
    (bind ?j 0)
    (printout t "current board :" crlf)
    (while (<= ?i 3) do
        (while (< ?j 3) do
            (bind ?result (run-query* search-coordinate-player i+j))
            (if(eq ?result X)
        		then (printout t "X ")
        		else (if (eq ?result X) 
                   	  	  then (printout t "O ")
                   		  else (printout t "- ")))
            (bind ?j (+ 1 ?j)))
        (printout t crlf)
        (bind ?j 0)
        (bind ?i (+ 1 ?i))
        )
    (retract ?i ?j)
    )

(defquery search-coordinate-player
    "query to find out if the player is in a specific square"
    (declare (variables ?sq))
    (occupied (square ?sq) (player ?pl))
    )

(defrule centre-square
"Rule 5 - play a centre square"
(not (occupied {square == 5}))
?phase <- (phase computer-move)
    =>
    (retract ?phase)
    (assert (phase human-move))
    (printout t "Playing centre" crlf)
    (bind ?who <- player-move) 
    (assert (occupied (square 5) (player ?who)))
    (printout t ?who crlf)
    (print-board()))

(defrule top-right-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 3}))
?phase <- (phase computer-move)
    =>
    (retract ?phase)
    (assert (phase human-move))
    (printout t "Playing upper right corner" crlf)
    (bind ?who <- player-move)
    (assert (occupied (square 3) (player ?who)))
    (print-board()))

/*
(defrule lower-right-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 9}))
?phase <- (phase computer-move)
    =>
    (retract ?phase)
    (assert (phase human-move))
    (printout t "Playing lower right corner" crlf)
    (assert (occupied (square 9) (player ?player-move)))
    (print-board()))

(defrule top-left-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 1}))
?phase <- (phase computer-move)
    =>
    (retract ?phase)
    (assert (phase human-move))
    (printout t "Playing upper left corner" crlf)
    (assert (occupied (square 1) (player ?player-move)))
    (print-board()))

(defrule lower-left-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 7}))
?phase <- (phase computer-move)
    =>
    (retract ?phase)
    (assert (phase human-move))
    (printout t "Playing lower left corner" crlf)
    (assert (occupied (square 7) (player ?player-move)))
    (print-board()))*/

(reset)
(run)
