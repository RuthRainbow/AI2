(deftemplate occupied
    "Square is occupied by player"
    (slot square(type INTEGER))
    (slot player(type STRING))
    )

(deffacts board
    (occupied (square 1) (player -))
    (occupied (square 2) (player -))
    (occupied (square 3) (player -))
    (occupied (square 4) (player -))
    (occupied (square 5) (player -))
    (occupied (square 6) (player -))
    (occupied (square 7) (player -))
    (occupied (square 8) (player -))
    (occupied (square 9) (player -))
    )

(deftemplate line
    "Winning line (3 in a row)"
    (slot sq1(type INTEGER))
    (slot sq2(type INTEGER))
    (slot sq3(type INTEGER))
    )

(deffacts lines
    (line (sq1 1) (sq2 2) (sq3 3))
	(line (sq1 4) (sq2 5) (sq3 6))
	(line (sq1 7) (sq2 8) (sq3 9))
	(line (sq1 1) (sq2 4) (sq3 7))
	(line (sq1 2) (sq2 5) (sq3 8))
	(line (sq1 3) (sq2 6) (sq3 9))
	(line (sq1 1) (sq2 5) (sq3 9))
	(line (sq1 3) (sq2 5) (sq3 7))
    /*
    (line (sq1 1) (sq2 3) (sq3 2))
	(line (sq1 4) (sq2 6) (sq3 5))
	(line (sq1 7) (sq2 9) (sq3 8))
	(line (sq1 1) (sq2 7) (sq3 4))
	(line (sq1 2) (sq2 8) (sq3 5))
	(line (sq1 3) (sq2 9) (sq3 6))
	(line (sq1 1) (sq2 9) (sq3 5))
	(line (sq1 3) (sq2 7) (sq3 5))
    
    (line (sq1 2) (sq2 1) (sq3 3))
	(line (sq1 5) (sq2 4) (sq3 6))
	(line (sq1 8) (sq2 7) (sq3 9))
	(line (sq1 4) (sq2 1) (sq3 7))
	(line (sq1 5) (sq2 2) (sq3 8))
	(line (sq1 6) (sq2 3) (sq3 9))
	(line (sq1 5) (sq2 1) (sq3 9))
	(line (sq1 5) (sq2 3) (sq3 7))
  
    (line (sq1 2) (sq2 3) (sq3 1))
	(line (sq1 5) (sq2 6) (sq3 4))
	(line (sq1 8) (sq2 9) (sq3 7))
	(line (sq1 4) (sq2 7) (sq3 1))
	(line (sq1 5) (sq2 8) (sq3 2))
	(line (sq1 6) (sq2 9) (sq3 3))
	(line (sq1 5) (sq2 9) (sq3 1))
	(line (sq1 5) (sq2 7) (sq3 3))

    (line (sq1 3) (sq2 2) (sq3 1))
	(line (sq1 6) (sq2 5) (sq3 4))
	(line (sq1 9) (sq2 8) (sq3 7))
	(line (sq1 7) (sq2 4) (sq3 1))
	(line (sq1 8) (sq2 5) (sq3 2))
	(line (sq1 9) (sq2 6) (sq3 3))
	(line (sq1 9) (sq2 5) (sq3 1))
	(line (sq1 7) (sq2 5) (sq3 3))
        
    (line (sq1 3) (sq2 1) (sq3 2))
	(line (sq1 6) (sq2 4) (sq3 5))
	(line (sq1 9) (sq2 7) (sq3 8))
	(line (sq1 7) (sq2 1) (sq3 4))
	(line (sq1 8) (sq2 2) (sq3 5))
	(line (sq1 9) (sq2 3) (sq3 6))
	(line (sq1 9) (sq2 1) (sq3 5))
	(line (sq1 7) (sq2 3) (sq3 5))
    */
    )

(deffacts initial-state
    (phase choose)
    )

(defrule player-select
    (phase choose)
    =>
    (printout t "Do you want to be X or O? ")
    (assert (human (read)))
    )

(defrule valid-player-select
    ?phase <- (phase choose)
    ?human <- (human ?player&:(or (eq ?player X) (eq ?player O)))
    =>
    (retract ?phase)
    (assert (phase choose-move) (move X))
    )
(defrule invalid-player-select
    ?phase <- (phase choose)
    ?human <- (human ?player&:(and (neq ?player X) (neq ?player O)))
    =>
    (retract ?phase ?human)
    (printout t "Invalid choice" crlf)
    (assert (phase choose))
    )

(defrule take-human-turn
    (phase choose-move)
    (human ?human)
    (move ?move&:(eq ?move ?human))
    =>
    (printout t "Where would you like to go? 1-9" crlf)
    (assert (input (read)))
    )

(defrule take-human-turn-valid
    ?phase <- (phase choose-move)
    (human ?human)
    (input ?sq&:(and (>= ?sq 1) (<= ?sq 9)))
    =>
    (retract ?phase)
    (assert (phase check-move))
    )

(defrule move-valid
    ?phase <- (phase check-move)
    ?in <- (input ?num)
    (move ?move)
    ?occ <- (occupied (square ?num) (player -))
    =>
    (retract ?phase ?in ?occ)
    (assert (phase turn-end) (occupied (square ?num) (player ?move)))
    )

(defrule move-invalid
    ?phase <- (phase check-move)
    ?in <- (input ?num)
    (not (occupied (square ?num) (player -)))
    =>
    (retract ?phase ?in)
    (printout t "This square is already taken" crlf)
    (assert (phase choose-move))
    )

(defrule take-human-turn-invalid
    ?phase <- (phase choose-move)
    (human ?human)
    ?in <- (input ?sq&:(or (< ?sq 1) (> ?sq 9)))
    =>
    (retract ?phase ?in)
    (printout t "Invalid choice" crlf)
    (assert (phase choose-move))
    )


(defrule take-AI-turn
    ?phase <- (phase choose-move)
    (human ?human)
    (move ?move&:(neq ?move ?human))
    =>
    (retract ?phase)
    (printout t "AI, take move..." crlf)
    (assert (phase AI1))
    )

(defquery 2-entry-line
    (declare (variables ?turn))
    (and
        (line (sq1 ?s1) (sq2 ?s2) (sq3 ?s3))
        (or 
            (and (occupied (square ?s1) (player ?turn)) (occupied (square ?s2) (player ?turn)))
            (and (occupied (square ?s2) (player ?turn)) (occupied (square ?s3) (player ?turn)))
            (and (occupied (square ?s1) (player ?turn)) (occupied (square ?s3) (player ?turn)))
            )
        )
    )

(defquery find-free
    ?occ <- (occupied (square ?num) (player -))
    )

(defrule take-AI-turn-1
    ?phase <- (phase AI1)
    (move ?move)
    =>
    (retract ?phase)
    (bind ?square (run-query* find-free))
    (bind ?continue TRUE)
    (while (?square next)
        (bind ?num (?square getInt num))
        (bind ?line (run-query* 2-entry-line ?move))
        (while (?line next)
            then
            (if (or(eq ?num (?line getInt s1)) (eq ?num (?line getInt s2)) (eq ?num (?line getInt s3)))
                then
                (retract (?square getObject occ))
                (bind ?continue FALSE)
                (assert (occupied (square ?num) (player ?move)) (phase turn-end))
                (printout t "MUHAHA Winner winner, chicken dinner" crlf)
                )
            )
        )
    (if ?continue then (assert (phase AI2)))
    )

(defrule take-AI-turn-2
    ?phase <- (phase AI2)
    (move ?move)
    (human ?human)
    =>
    (retract ?phase)
    (bind ?square (run-query* find-free))
    (bind ?continue TRUE)
    (while (?square next)
        (bind ?num (?square getInt num))
        (bind ?line (run-query* 2-line ?human))
        (while (?line next)
            then
            (if (or(eq ?num (?line getInt s1)) (eq ?num (?line getInt s2)) (eq ?num (?line getInt s3)))
                then
                (retract (?square getObject occ))
                (bind ?continue FALSE)
                (assert (occupied (square ?num) (player ?move)) (phase turn-end))
                (printout t "YOU SHALL NOT PASS" crlf)
                )
            )
        )
    (if ?continue then (assert (phase AI3)))
    )

(defrule take-AI-turn-3
    ?phase <- (phase AI3)
    (move ?move)
    =>
    (retract ?phase)
    (assert (phase AI4))
    )


(defrule take-AI-turn-4
    ?phase <- (phase AI4)
    (move ?move)
    (human ?human)
    =>
    (retract ?phase)
    (assert (phase AI5))
    )


(defrule take-AI-turn-5
    ?phase <- (phase AI5)
    (move ?move)
    ?occ <- (occupied (square 5) (player -))
    =>
    (retract ?phase ?occ)
    (assert (occupied (square 5) (player ?move)) (phase turn-end))
    (printout t "Center for me :D" crlf)
    )

(defrule take-AI-turn-5-fail
    ?phase <- (phase AI5)
    (not (occupied (square 5) (player -)))
    =>
    (retract ?phase)
    (assert (phase AI6))
    )

(defquery find-free-corner
    ?occ <- (occupied (square ?num&:(or (eq ?num 1) (eq ?num 3) (eq ?num 7) (eq ?num 9))) (player -))
    )

(defrule take-AI-turn-6
    ?phase <- (phase AI6)
    (move ?move)
    =>
    (retract ?phase)
    (bind ?free (run-query* find-free-corner))
    (if (?free next)
        then
        (retract (?free getObject occ))
        (assert (occupied (square (?free getInt num)) (player ?move)) (phase turn-end))
        (printout t "Corner smorner =[" crlf)
        else
        (assert (phase AI7))
        )
    )


(defquery find-free
    ?occ <- (occupied (square ?num) (player -))
    )

(defrule take-AI-turn-7
    ?phase <- (phase AI7)
    (move ?move)
    =>
    (retract ?phase)
    (bind ?free (run-query* find-free))
    (if (?free next)
        then
        (retract (?free getObject occ))
        (assert (occupied (square (?free getInt num)) (player ?move)) (phase turn-end))
        (printout t "I'll take what I can get." crlf)
        else
        (printout t "AI COULD NOT MOVE, ERROR =[" crlf)
        )
    )

(defrule turn-end-x
    ?phase <- (phase turn-end)
    ?move <- (move X)
    =>
    (printout t "X end" crlf)
    (retract ?phase ?move)
    (assert (phase print-board) (move O))
    )

(defrule turn-end-o
    ?phase <- (phase turn-end)
    ?move <- (move O)
    =>
    (printout t "O end" crlf)
    (retract ?phase ?move)
    (assert (phase print-board) (move X))
    )

(defrule print-board
    ?phase <- (phase print-board)
    =>
    (retract ?phase)
    (assert (input 0) (phase print-board-count))
    )

(defrule print-board-count
    ?phase <- (phase print-board-count)
    ?input <- (input ?num)
    =>
    (retract ?phase ?input)
    (bind ?num (+ ?num 1))
    (if (eq (mod ?num 3) 1)
        then (printout t crlf))
    (if (< ?num 10)
        then
        (assert (input ?num) (phase print))
        else
        (assert (phase choose-move))
        )
    )

(defrule print
    ?phase <- (phase print)
    ?in <- (input ?i)
    (occupied (square ?i) (player ?c))
    =>
    (retract ?phase)
    (printout t ?c " ")
    (assert (phase print-board-count))
    )

(defrule game-end
    (not (occupied (square ?) (player -)))
    =>
    (printout t "No more moves!" crlf)
    (ask-start-again())
    )

(deffunction ask-start-again ()
    (printout t "Play again? (y/n) ")
    (if (eq (read) y)
        then
        (reset)
        else
        (halt)
        )
    )
    
    (reset)
    (run)
