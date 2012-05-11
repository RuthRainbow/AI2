(deftemplate line
"Winning line (3 in a row)"
(slot sq1(type INTEGER))
(slot sq2(type INTEGER))
(slot sq3(type INTEGER))
)

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
   (assert (player-select (read))))

(defrule valid-choice
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&:(or (eq ?player X) (eq ?player O)))
   =>
   (retract ?phase ?choice)
   (assert (player-move X))
    )

(defrule invalid-choice 
   ?phase <- (phase choose-player)
   ?choice <- (player-select ?player&~X&~O)
   =>
   (retract ?phase ?choice)
   (assert (phase choose-player))
   (printout t "Please choose X or O." crlf))

	(assert (line (sq1 1) (sq2 2) (sq3 3)))
	(assert (line (sq1 4) (sq2 5) (sq3 6)))
	(assert (line (sq1 7) (sq2 8) (sq3 9)))
	(assert (line (sq1 1) (sq2 4) (sq3 7)))
	(assert (line (sq1 2) (sq2 5) (sq3 8)))
	(assert (line (sq1 3) (sq2 6) (sq3 9)))
	(assert (line (sq1 1) (sq2 5) (sq3 9)))
	(assert (line (sq1 3) (sq2 5) (sq3 7)))

	(defquery search-coordinate
	 "query to find out all the points that X/O occupies:"
	 (declare (variables ?pl))
	 (occupied (square ?sq) (player ?pl))
	)
	 
	 (bind ?result (run-query* search-coordinate X))
	 
	 (defquery search-coordinate-player
	 "query to find out if the player is in a specific square"
	 (declare (variables ?sq))
	 (occupied (square ?sq) (player ?pl))
	    )
	   
	   defrule centre-square
	   "Rule 5 - play a centre square"
	   ((not occupied {square = 5})
	   =>
	   (assert (occupied (square 5) (player ?p))))
	
	
	
	defrule top-right-corner
	   "Rule 6 - play an available corner square"
	((not occupied {square = 3})
	=>
	 (assert (occupied (square 3) (player ?p))))
	
	defrule lower-right-corner
	   "Rule 6 - play an available corner square"
	((not occupied {square = 9})
	=>
	 (assert (occupied (square 9) (player ?p))))
	
	defrule top-left-corner
	   "Rule 6 - play an available corner square"
	((not occupied {square = 1})
	=>
	 (assert (occupied (square 1) (player ?p))))
	
	defrule lower-left-corner
	   "Rule 6 - play an available corner square"
	((not occupied {square = 7})
	=>
	 (assert (occupied (square 7) (player ?p))))


(reset)
(run)