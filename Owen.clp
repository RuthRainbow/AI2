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

(deffacts adjacent
    (adjacent 1 2)
    (adjacent 1 4)
    (adjacent 1 5)
    
    (adjacent 2 1)
    (adjacent 2 3)
    (adjacent 2 5)
    
    (adjacent 3 2)
    (adjacent 3 6)
    (adjacent 3 5)
    
    (adjacent 4 5)
    (adjacent 4 1)
    (adjacent 4 7)
    
    (adjacent 5 1)
    (adjacent 5 2)
    (adjacent 5 3)
    (adjacent 5 4)
    (adjacent 5 6)
    (adjacent 5 7)
    (adjacent 5 8)
    (adjacent 5 9)
    
    (adjacent 6 5)
    (adjacent 6 3)
    (adjacent 6 9)
    
    (adjacent 7 8)
    (adjacent 7 4)
    (adjacent 7 5)
    
    (adjacent 8 7)
    (adjacent 8 9)
    (adjacent 8 5)
    
    (adjacent 9 8)
    (adjacent 9 6)
    (adjacent 9 5)
    )

(deftemplate line
    "Winning line (3 in a row)"
    (slot sq1(type INTEGER))
    (slot sq2(type INTEGER))
    (slot sq3(type INTEGER))
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

(defrule take-AI-turn-1
    ?phase <- (phase AI1)
    =>
    (retract ?phase)
    (assert (phase AI2))
    )

(defrule take-AI-turn-2
    ?phase <- (phase AI2)
    =>
    (retract ?phase)
    (assert (phase AI3))
    )

(defquery find-adjacent
    (declare (variables ?num ?turn))
    (adjacent ?num ?numadj)
    (occupied (square ?numadj) (player ?turn))
    )

(deffunction boolean-double (?num ?turn)
    (bind ?taken (run-query* find-adjacent ?num ?turn))
    (if (and (?taken next) (?taken next))
        then
        (return true)
        )
    )

(defquery find-double-free
    (declare (variables ?turn))
    (and
        ?occ <- (occupied (square ?num) (player -))
        (boolean-double ?num ?turn)
        )
    )

(defrule take-AI-turn-3
    ?phase <- (phase AI3)
    (move ?move)
    =>
    (retract ?phase)
    (bind ?double (run-query* find-double-free ?move))
    (if (?double next)
        then
        (retract (?double getObject occ))
        (assert (occupied (square (?double getInt num)) (player ?move)) (phase turn-end))
        (printout t "DOUBLE RAINBOW! :D" crlf)
        else
        (assert (phase AI4))
        )
    (assert (phase AI4))
    )


(defrule take-AI-turn-4
    ?phase <- (phase AI4)
    (move ?move)
    (human ?human)
    =>
    (retract ?phase)
    (bind ?double (run-query* find-double-free ?human))
    (if (?double next)
        then
        (retract (?double getObject occ))
        (assert (occupied (square (?double getInt num)) (player ?move)) (phase turn-end))
        (printout t "NO DOUBLE RAINBOWS! >=[" crlf)
        else
        (assert (phase AI5))
        )
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


;(deffunction freeCorner ()
;   (foreach ?i (create$ 1 3 7 9)
;       (if (test (not (occupied (square ?i) (player ?))))
;            then (return ?i))
;       )
;   (return 0)
;   )

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
