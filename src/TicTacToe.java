/*
 * Author: Nathan Griffiths
 * Version: 20110220
 *
 * TicTacToe: The main class implementing minimax and alpha-beta
 * pruning.
 *
 * Notes:
 *     CS239 KBS Coursework: minimax and alpha-beta pruning. This code
 *     is deliberately minimally commented, written for simplicity,
 *     and does not always follow good coding practise. However, it
 *     should be very easy to follow if you understand minimax
 *     and alpha-beta pruning. Loosely based on code by Ravi Mohan.
 */

import java.util.*;

public class TicTacToe {

    GameState gameState;      // Current game state
    int totalNodesExpanded;   // Total number of nodes expanded in
			      // search so far
    int levelNodesExpanded;   // Number of nodes expanded to explore this
			      // level of the tree (i.e. all
			      // non-pruned successors of the current
			      // state) 
    int alpha;                // Value of best choice found so far for
			      // Max
    int beta;                 // Value of best choice found so far for
			      // Min 

    /*
     * Simple constructor that initializes current player and the
     * moves set to be all possible moves.
     */
    public TicTacToe() {
	gameState = new GameState();
	ArrayList<Coordinate> moves = new ArrayList<Coordinate>();
	for (int i = 0; i < 3; i++) {
	    for (int j = 0; j < 3; j++) {
		moves.add(new Coordinate(i,j));
	    }
	}
	gameState.player = "X";
	gameState.moves = moves;
	totalNodesExpanded = 0;
	levelNodesExpanded = 0;
    }

    /*
     * Return a list of the states reachable in one move from the
     * current state.
     */
    public ArrayList getSuccessorStates(GameState state) {
	ArrayList<GameState> result = new ArrayList<GameState>();

	for (int i = 0; i < state.moves.size(); i++) {
	    Coordinate c = (Coordinate) state.moves.get(i);
	    GameState s = makeMove(state, c);
	    s.moveMade = c;
	    result.add(s);
	}
	return result;
    }

    /*
     * Perform a move to "c" from the given "state" (checking that the
     * move is valid in the current state) 
     */
    public GameState makeMove(GameState state, Coordinate c) {
	GameState result = null;
	ArrayList newMoves = (ArrayList) state.moves.clone();
	if (newMoves.contains(c)) {
	    newMoves.remove(c);
	    result = new GameState();
	    result.moves = newMoves;
	    TicTacToeBoard newBoard = (TicTacToeBoard) state.board.clone();
	    if (state.player == "X") {
		newBoard.markX(c.x, c.y);
		result.player = "O";
	    } else {
		newBoard.markO(c.x, c.y);
		result.player = "X";
	    }
	    result.board = newBoard;
	} else {
	    System.out.println("ERROR: attempt to make invalid move");
	}
	return result;
    }

    /*
     * Make the next move, determined using minimax.
     * Print out information about the search progression.
     */
    public void makeMiniMaxMove() {
	totalNodesExpanded += levelNodesExpanded;
	levelNodesExpanded = 0;

	// Fill in the code here to do the next move using minimax
	// (with no alpha-beta pruning)

	// You may introduce additional methods, but you must retain
	// makeMiniMaxMove as the only method that needs calling from
	// elsewhere, and you may not introduce additional classes.

	// You must comment your code to explain what you have done.

	// Print how many nodes were expanded to find this move
	System.out.println("Nodes considered: " + levelNodesExpanded);

	// Print the move itself - YOU SHOULD FILL THIS IN TOO
	System.out.println("Move chosen: " );
	// E.g.:
	// System.out.println("Move chosen: " + nextState.moveMade);
    }

    /*
     * Make the next move, determined using minimax with alpha-beta
     * pruning.
     * Print out information about the search progression.
     */
    public void makeAlphaBetaMove() {
	totalNodesExpanded += levelNodesExpanded;
	levelNodesExpanded = 0;

	// Fill in the code here to do the next move using minimax
	// with alpha-beta pruning

	// You may introduce additional methods, but you must retain
	// makeAlphaBetaMove as the only method that needs calling from
	// elsewhere, and you may not introduce additional classes.

	// You must comment your code to explain what you have done.

	// Print how many nodes were expanded to find this move
	System.out.println("Nodes considered: " + levelNodesExpanded);
	// Print the move itself - YOU SHOULD FILL THIS IN TOO
	System.out.println("Move chosen: " );
	// E.g.:
	// System.out.println("Move chosen: " + nextState.moveMade);
    }

    public static void main(String[] args) {

	/********************************************************
	 * The following code is just to illustrate how to use the
	 * board. Uncomment it to play around. However, IT SHOULD BE
	 * COMMENTED OUT IN YOUR SUBMISSION.
	 ********************************************************/
	/*
	// Create a new board, print it, print the possiblemoves, and
	// show what states could be acheived
	TicTacToe board1 = new TicTacToe();
	System.out.println("board:\n" + board1.gameState.board);
	System.out.println("moves: " + board1.gameState.moves);

	ArrayList possMoves = board1.getSuccessorStates(board1.gameState);
	for (int i = 0; i < possMoves.size(); i++) {
	    System.out.println(((GameState) possMoves.get(i)).moveMade);
	    System.out.println(((GameState) possMoves.get(i)).board);
	}
	*/

	/********************************************************
	 * The following code is to illustrate how your submission
	 * will be used (although other tests will also be
	 * performed). Until you have implemented makeMiniMaxMove and
	 * makeAlphaBetaMoves, it clearly will not work. However,
	 * once you've written these methods it should produce output
	 * of the form shown at the end of this file.
	 ********************************************************/

		// Use standard minimax on an empty board
		System.out.println("--- Standard minimax ---");
		TicTacToe board1 = new TicTacToe();
		System.out.println("board:\n" + board1.gameState.board + "\n");
		while (!(board1.gameState.board.lineExists() ||
			 board1.gameState.board.boardFull())) {
		    System.out.println("Current player: " + board1.gameState.player);
		    System.out.println("Possible moves: " + board1.gameState.moves);
		    board1.makeMiniMaxMove();
		    System.out.println("board:\n" + board1.gameState.board + "\n");
		}
		System.out.println("Total nodes expanded: " 
				   + board1.totalNodesExpanded);
    }
}
