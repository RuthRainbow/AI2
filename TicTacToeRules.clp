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

(deftemplate player-turn
    "Square is occupied by player"
    (slot go(type STRING))
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
    (bind ?player-turn <- first-player(?choice))
    (retract ?phase ?choice)
    (assert (player-move X))
    )

(deffunction first-player(?move)
    (if (eq ?move X) then
        (return h)
     else
        (return c))
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
        (return X)
     else
        (return O)))

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

(defrule centre-square
"Rule 5 - play a centre square"
(not (occupied {square == 5}))
(player-turn {go == c})
    =>
    (bind ?player-turn <- (modify(player-turn (go (next-player(?player-turn))))))
    (printout t "Playing centre" crlf)
    (assert (occupied (square 5) (player player-move))))

(defrule top-right-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 3}))
(player-turn (go c))
    =>
    (bind ?player-turn <- (modify(player-turn (go (next-player(?player-turn))))))
    (printout t "Playing top right" crlf)
    (assert (occupied (square 3) (player player-move))))

(defrule lower-right-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 9}))
(player-turn (go c))
    =>
    (bind ?player-turn <- (modify(player-turn (go (next-player(?player-turn))))))
    (printout t "Playing lower right" crlf)
    (assert (occupied (square 9) (player player-move))))

(defrule top-left-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 1}))
(player-turn (go c))
    =>
    (bind ?player-turn <- (modify(player-turn (go (next-player(?player-turn))))))
    (printout t "Playing top left" crlf)
    (assert (occupied (square 1) (player player-move))))

(defrule lower-left-corner
"Rule 6 - play an available corner square"
(not (occupied {square == 7}))
(player-turn (go c))
    =>
    (bind ?player-turn <- (modify(player-turn (go (next-player(?player-turn))))))
    (printout t "Playing lower left" crlf)
    (assert (occupied (square 7) (player player-move))))

(reset)
(run)
