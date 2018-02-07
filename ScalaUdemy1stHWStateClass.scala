package fileSystem

//import java.io._  //for creating files

class State(val root: Directory, val wd: Directory, val output: String) {
	def showShell: Unit =
		print(State.SHELL_TOKEN)

	//console interface
	def show: Unit = {
		println(output)
		showShell
	}

	//builds NEW STATE and logs system message
	def withMessage(message: String): State =
		new State(root, wd, message)
	
	/* acc is to be null for first iteration; process for absolute path
	converts a relative path into it's absolute equivalent
	*/
	@tailrec
	def absUtil(remPath: Array[String], acc: String = null): String = {
		if (remPath.isEmpty) {
			acc
		}
		else {
			remPath(0) match {
				case "" => {
					// root path at beginning
					val newacc = root.getDirPath
					absUtil(remPath.drop(1), newacc)
				}
				case Object.PARENT => {
					// replace pwd with pwd's parent path
					if (acc === null) {
						absUtil(remPath.drop(1), wd.parentPath)
					}
					else {
						val newacc = acc.split(Object.SEPARATOR).dropRight(1)
						absUtil(remPath.drop(1), newacc.mkString("/"))
					}
				}

				case _ => {
					// traverse and add to acc
					if (remPath(0) == Object.PWD) {
						// do nothing and traverse
						if (acc === null) {
							absUtil(remPath.drop(1), wd.getDirPath)
						}
						else {
							absUtil(remPath.drop(1), acc)
						}
					}
					else {  //name of a direntry
						if (acc === null) {
							val newacc = wd.getDirPath + Object.SEPARATOR + remPath(0)
							absUtil(remPath.drop(1), newacc)
						}
						else {
							val newacc = acc + Object.SEPARATOR + remPath(0)
							absUtil(remPath.drop(1), newacc)
						}
					}
				}
			}
		}
	}


	def cdState(path: String): State = {
		
		// process the path, if path starts with '/' then it'll have "" as first array[string] element
		val disPath: Array[String] = path.split(Directory.SEPARATOR)
		val absPath: String = absUtil(disPath)

		val newWd = root.findChildDir(absPath)
		new State(root, newWd, "")
	}
	def mkdirState(dirName: String, message: String) = {
		val x = wd.getDirPath + Directory.SEPARATOR + dirName
		val newRoot = Directory.newPath(x, root)
		val newWd: DirEntry = newRoot.findChildDir(wd.getDirPath)

		if (newWd === null){
			// throw an error
		}
		else {
			new State(newRoot, newWd, message)
		}
	}

	def lsState = {
		val chil: List[DirEntry] = wd.getChildren
		chil.foreach(println(_.name))

		val message = "TOTAL: " + chil.length + " ENTRIES"
		new State(root, wd, message)
	}

	def rmState(entry: String, message: String) = {
		val newRoot = wd.rmDirEntry(entry, root)

		if (newRoot === null){
			// throw error, wasn't able to find child with given name
		}
		else {
			val newWd = newRoot.findChildDir(wd.getDirPath)

			if (newWd === null) {
				// throw an error
			}
			else {
				new State(newRoot, newWd, message)
			}
		}
		
	}

	def echoState(phrase: String): State = {
		val WRITE = ">"
		val str: Array[String] = phrase.trim.split(WRITE)

		/***
			3 cases:
			dangling > at end => "" in str(str.length-1)
			file name => write to file
			last elem is a string => write to console

			*** a >> will produce a "" elem to separate 2 string elems
			ignore any string(s) after a file name
		***/
		@tailrec
		def getFinalString(s: Array[String], appFlag: Boolean, acc: String): String = {
			if (s.isEmpty){
				acc
			}
			else {
				if (appFlag){
					getFinalString(s.drop(1), false, acc + "\n" + s(0))
				}
				else if (s(0) == ""){
					getFinalString(s.drop(1), true, acc)
				}
				else{
					getFinalString(s.drop(1), false, s(0))
				}
			}
		}

		

		val Patt: Regex = "[a-zA-Z-_]+\.[a-zA-Z]+".r
		
		str(str.length-1).toLowerCase match {
			case "" => {
				
				new State(root, wd, "Error: Invalid Argument(s)")
			}
			case Patt(c) => {  // write to a file

				if (str.(str.length-2) == ""){
					// append finalStr to file; only valid if file exists
					val child = wd.find(c)

					if (child === null) {
						// throw error: no such file
						new State(root, wd, "No such file exists to append to")
					}
					else {
						val finalStr = getFinalString(str.dropLast(1), false, null)
						val pa = wd.getDirPath + Directory.SEPARATOR + c
						val file = File.findFile(pa, root)
						val comfinalStr = file.getContents + "\n" + finalStr  // need to get contents of old file
						val n = Directory.newPath(pa, root, new File(wd.getDirPath, c, comfinalStr))
						val w: DirEntry = n.findChildDir(wd.getDirPath)
						
						new State(n, w, "File "+ c + ": updated")
					}

				}
				else{
					// overwrite/create file
					val finalStr = getFinalString(str.dropLast(1), false, null)
					
					val pa = wd.getDirPath + Directory.SEPARATOR + c
					val n = Directory.newPath(pa, root, new File(wd.getDirPath, c, finalStr))
					val w: DirEntry = n.findChildDir(wd.getDirPath)
					
					new State(n, w, "File "+ c + ": updated")

				}
			}
		}
	}

	def catState(fileName: String): State = {
		val filePath = wd.getDirPath + Directory.SEPARATOR + fileName
		val file = File.findFile(filePath, root)

		println(file.getContents)
		withMessage("File "+ fileName+": Printed " + file.getContents.length + " chars long")
	}

	def touchState(fileName: String): State = {
		val file = File.empty(wd.getDirPath, fileName)

		val pa = wd.getDirPath + Directory.SEPARATOR + fileName
		val n = Directory.newPath(pa, root, file)
		val w: DirEntry = n.findChildDir(wd.getDirPath)
		
		new State(n, w, "Empty file "+ fileName + ": created")

	}

	def mvState(entryPath: String, destPath: String) = {
		// make absolute path
		val entry: Array[String] = entryPath.split(Directory.SEPARATOR)
		val absEntryPath: String = absUtil(entry)
		val dest = destPath.split(Directory.SEPARATOR)
		val absDestPath: String = absUtil(dest)

		// find last common parent in both paths
		val e = absEntryPath.split(Directory.SEPARATOR)
		val d = absDestPath.split(Directory.SEPARATOR)

		@tailrec
		def common(e: Array[String], d: Array[String], acc: Int = 0): Int = {
			if (e.isEmpty || d.isEmpty){
				acc
			}
			else if (e(0) == d(0)){
				common(e.drop(1),d.drop(1), acc++)
			}
			else {
				acc
			}
		}

		val commIndex: Int = common(e, d)
		val buff: DirEntry
		// try block to return a direntry node
		try {
			// get common parent of both paths
			val commParent: String = e(commIndex-1)

			// cut and paste the direntry from one path to the destPath


			/*
			then update the individual paths, return pointers for each updated path
			then create a new instance of commParent, connect the pointers and get newRoot
			with new commParent
			*/
			val targetDir = root.findChildDir(absEntryPath)
			val targetParent = root.findChildDir(e.dropRight(1).mkString(Directory.SEPARATOR))
			val a = root.findChildDir(e.dropRight(length-1-commIndex).mkString(Directory.SEPARATOR))
			val b = root.findChildDir(d.dropRight(length-1-commIndex).mkString(Directory.SEPARATOR))
			
			val newTargetPath = targetParent.rmDirEntry(targetDir.name, a, Directory.SEPARATOR+(e.drop(commIndex).mkString(Directory.SEPARATOR)))
			val newDestPath = Directory.newPath(Directory.SEPARATOR+(d.drop(commIndex).mkString(Directory.SEPARATOR)), b, targetDir)
		
			val newRoot = Directory.newPath(a.getDirPath, root, newTargetPath)
			val finalRoot = Directory.newPath(b.getDirPath, newRoot, newDestPath)
		} catch {
			//case n: NullPointerException =>
			case e: Exception => {
				buff = new File("","",e.printStackTrace().toString())
				
				// *** note that this return value has to be the same as try block's
			}
		} finally {
			// for all the side-effects
			println(buff.contents)
		}
		


	}

}

object State {
	val SHELL_TOKEN = "$ "

	//factory method: builds the states
	def apply(root: Directory, wd: Directory, output: String = ""): State =
		new State(root, wd, output)
}