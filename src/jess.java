import java.util.*;
import jess.*;

public class jess {
	private Rete engine;
	private WorkingMemoryMarker marker;
	
	public jess() throws JessException {
		//Create a jess rule engine
		engine = new Rete();
		engine.reset();
		
		//Load the tic tac toe rules
		engine.batch("TicTacToeRules.clp");
		
		marker = engine.mark(); //not sure if we need this
		
	}	
	
}