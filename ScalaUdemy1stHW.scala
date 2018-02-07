package fileSystem
//import util.control.Breaks._
/*import org.scalatest._
import Matchers._*/


abstract class DirEntry(val parentPath: String, val name: String) {}

class Directory(
	override val parentPath: String,
	override val name: String,
	val contents: List[DirEntry]) extends DirEntry(parentPath, name) {

	

	//add code here
	def getDirPath: String = parentPath + Directory.SEPARATOR + name
	def getChildren: List[DirEntry] = contents
	def find(name: String): DirEntry = {
		val res: Array[DirEntry] = this.contents.filter(elem => elem.name == name)
		if (!res.isEmpty)
			res(0)
		else
			null
	}

	/* returns the newRoot created if a directory is removed */
	def rmDirEntry(entry: String, root: Directory, arbitrate: String = null): Directory = {
		val res: DirEntry = find(entry)
		if (res === null) {
			null
		}
		else {  // with the entry found, remove it and update path
			val newCon: List[DirEntry] = contents.flatMap(x => if (x === res) None else Some(x))
			val newCurrDir = new Directory(parentPath, name, newCon)
			if (arbitrate === null){
				Directory.newPath(getDirPath, root, newCurrDir)
			}else{
				Directory.newPath(arbitrate, root, newCurrDir)
			}
		}
	}
	
	// pass in any string with this as the reference root; path is abs path
	def findChildDir(path: String) = {
		// look through subDirs to find dir
		@tailrec
		def util(path: Array[String], currObj: DirEntry): DirEntry = {
			if (path.length <= 1){
				if (path(0) == currObj.name){
					currObj
				}
				else {
					null  //obj not found
				}
			}
			else if (currObj === null) {
				// we haven't arrived at the last directory but currObj = null
				//throw an error since we couldn't find the directory
				null
			}
			else {
				val l: Array[DirEntry] = currObj.contents.filter(x => x.name == path(1))
				if (l.isEmpty) {
					util(path.drop(1), null)
				}
				else{
					util(path.drop(1), l(0))
				}
			}	
		}

		util(path.split(SEPARATOR).drop(1), this)  //drop(1) to account for root's parentPath
	}


}

object Directory {
	val SEPARATOR: String = "/"
	val ROOT_PATH: String = "/"
	val PARENT: String = ".."
	val PWD: String = "."

	//util functions
	def empty(parentPath: String, name: String) = new Directory(parentPath, name, List())  //returns empty directory
	
	def newRoot: Directory = Directory.empty("", "")  //creates root directory
	
	// path takes full new path with new dir/or file; for files use name with extention in path
	def newPath(path: String, root: Directory, replaceDir: DirEntry = null): Directory = {
		val tokens: Array[String] = path.split(SEPARATOR)

		
		def util(currpath: Array[String], currOldParent: Directory): Directory = {
			
			// copy constructor that copies everything and replaces 1 target dirEntry
			def copyContents (currpath: Array[String], currOldParent: Directory, target: String): List[DirEntry] = {
				val newContents: List[DirEntry] = currOldParent.contents.flatMap(elem => if (elem.name == target) None else Some(elem))
		
				if (currpath.length <= 2) {
					util(currpath.drop(1), null) :: newContents  // :: needs the elem type to be in front; ::: accepts 2 lists in any order
				}
				else {
					val nextNode = currOldParent.contents.filter(x => x.name == target)
					util(currpath.drop(1), nextNode(0)) :: newContents  // :: needs the elem type to be in front; ::: accepts 2 lists in any order
				}

				
			}


			if (currpath.length == 1){
				//return empty directory
				val parentPath = path //path.mkString("/")
				val newDirName = currpath(0)
				if (replaceDir === null)
					empty(parentPath, newDirName)
				else
					// add a predefined directory at the end

					replaceDir
			}
			else {
				
				if (currpath(0) != currOldParent.name) {
					// throw an exception
				}
				else {
					val newCon = copyContents(currpath, currOldParent, currpath(1))
					new Directory(currOldParent.parentPath, currOldParent.name, newCon)
				}

			}
		}

		util(tokens.drop(1), root)
		
	}

}

class File(
	override val parentPath: String,
	override val name: String,
	val contents: String) extends DirEntry(parentPath, name) {

	def getContents: String = contents
}

object File {
	def empty(parentPath: String, name: String) =
		new File(parentPath, name, "")

	def findFile(path: String, root: Directory): File = {
		root.findChildDir(path)
	}

}

