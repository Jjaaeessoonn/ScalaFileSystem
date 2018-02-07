package fileSystem
import scala.util.matching.Regex
import scala.util.control.Breaks.{break, brakable}

trait Command {
	def apply(state: State): State
}

object Command {
	def unknownCommand(name: String): Command =
		new Command {
			override def apply(state: State): State =
				state.withMessage(name + ": command not found")
		}

	// list of commands:	
	def mkdir(childDir: String) = {
		// make a new Dir Object, check if it's a file or directory
		val dirPatt: Regex = "[a-zA-Z-_]+".r
		
		childDir match {
			case dirPatt(c) => {
				// dir
				new Command {
					override def apply(state: State): State =
						state.mkdirState(c, c + " directory created")
				}
			}
			case _ => {
				new Command {
					override def apply(state: State): State =
						state.withMessage(c + " is not a valid directory name!")
				}
				// **** TODO: throw an error in the future
			}
		}
	}

	def cd(path: String) =
		/*
		changes wd; path can be absolute or relative to pwd;
		supports '.' & '..'
		*/
		new Command {
			override def apply(state: State): State = 
				state.cdState(path)
		}


	def ls =
		/*
		displays children of pwd
		*/
		new Command {
			override def apply(state: State): State =
				state.lsState
		}

	def rm(entry: String) = {
		/*
		receives a DirEntry to remove as argument
		*/
		new Command {
			override def apply(state: State): State =
				state.rmState(entry, "Remove DirEntry: " + entry + "...")
		}
	}

	def echo(phrase: String) = {
		/*
		displays something to console. Can be used to write to files,
		i.e. "$ echo hello world > file.txt"
		args: 1 or more
		*/
		new Command {
			override def apply(state: State) =
				state.echoState(phrase)
		}
		
	}

	def cat(fileName: String) = {
		/* display file contents to console */
		new Command {
			override def apply(state: State) =
				state.catState(fileName)
		}
	}

	def touch(fileName: String) = {
		/* create empty file with given name */
		new Command {
			override def apply(state: State) =
				state.touchState(fileName)
		}
	}

	def mv(entryPath: String, destPath: String) = {
		/* both paths include file/directory names and can be absolute/relative path */
		//first disconnect entryPath, update to newRoot
		//then use the updated structure and add entryPath at dest and create a newRoot
		//make sure that we can only pick subdirectories of pwd to move

		new Command {
			override def apply(state: State) =
				state.mvState(entryPath, destPath)
		}

	}


	//this is this classes "apply" function
	def from(input: String): Command = {
		val tokens = input.trim.split(" ")  //this parses the commands and arguments into an array of strings and trim takes care of leading/lagging extra spaces
		
		tokens(0).toLowerCase match {
			case "mkdir" => mkdir(tokens(1))
			case "cd" => cd(tokens(1))
			case "ls" => ls
			
			case "rm" => 
			{
				// *** do some error checking first
				rm(tokens(1))
			}
			case "echo" => echo(tokens(1))
			case "cat" => cat(tokens(1))
			case "touch" => touch(tokens(1))
			
			case "mv" => mv(tokens(1),tokens(2))
			case _ => unknownCommand(tokens(0))  //creates a command object with a message

		}
		
	}
}
