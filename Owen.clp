(deftemplate occupied
   "Square is occupied by player"
   (slot square(type INTEGER))
   (slot player(type STRING))
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
   (printout t "Do you want the be X or O? ")
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
   (not (square ?num ?))
   =>
   (retract ?phase ?in)
   (assert (phase turn-end) (square ?num ?move))
   )

(defrule move-invalid
   ?phase <- (phase check-move)
   ?in <- (input ?num)
   (square ?num ?)
   =>
   (retract ?phase ?in)
   (printout t "This square is already taken")
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


(defrule take-AI-turn-3
   ?phase <- (phase AI3)
   =>
   (retract ?phase)
   (assert (phase AI4))
   )


(defrule take-AI-turn-4
   ?phase <- (phase AI4)
   =>
   (retract ?phase)
   (assert (phase AI5))
   )


(defrule take-AI-turn-5
   ?phase <- (phase AI5)
   ?move <- (move ?)
   (not (square 5 ?))
   =>
   (retract ?phase)
   (assert (square 5 ?move) (phase turn-end))
   )

(defrule take-AI-turn-5-fail
   ?phase <- (phase AI5)
   (square 5 ?)
   =>
   (retract ?phase)
   (assert (phase AI6))
   )


(deffunction freeCorner ()
   (foreach ?i (create$ 1 3 7 9)
       (if (not (square ?i ?)) then (return ?i))
       )
   (return 0)
   )

(defquery find-free-corner
   (declare (variables ?ln))
   (square ?num&:(or (eq ?num 1) (eq ?num 3) (eq ?num 7) (eq ?num 9)) ?)
   )

(defrule take-AI-turn-6
   ?phase <- (phase AI6)
   =>
   (retract ?phase)
   (bind ?num (freeCorner))
   (if (neq ?num 0)
       then
       (assert (square ?num ?move) (phase turn-end))
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
   (assert (phase choose-move) (move O))
    (print-board())
   )

(defrule turn-end-o
   ?phase <- (phase turn-end)
   ?move <- (move O)
   =>
   (printout t "O end" crlf)
   (retract ?phase ?move)
   (assert (phase choose-move) (move X))
    (print-board())
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

(reset)
(run)