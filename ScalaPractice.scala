object Main extends App {
	var x: Int = 1+9
	def my_Func(x: Int, y: String): Unit = {
	  x+y
	}
}


import Practice._  //iport object identifiers to class
class Practice (x: Int, y: String){
	//some stuff
	def getMsg: String = msg
}
object Practice {  //object stores all static functions
	val msg: String = "Greetings!"
	def apply(x:Int, y:String): Practice = new Practice(x,y)  //equivalent to a constructor

}
