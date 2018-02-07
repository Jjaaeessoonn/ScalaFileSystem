package fileSystem

object Filesystem extends App {
	val firstRoot = Directory.newRoot  //create initial root
	var state = State(firstRoot, firstRoot)  //creates state for root
	val scanner = new Scanner(System.in)  //setup prompt for user input
	
	while(true) {
		state.show
		state = Command.from(scanner.nextLine()).apply(state)  //cmd object returned from "from" method, pass in state to create next state
	}
}