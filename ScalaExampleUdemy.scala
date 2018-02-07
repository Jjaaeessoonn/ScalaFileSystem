import scala.annotation.tailrec

//singleton object: Main function
object ListDemo extends App {
	val listA = Nil
	val listB = new Cons(2, new Cons(4, Nil))

	println(listA)  //calls listA.toString
	println(listB)

	println(listB.reverse)
	//println(listA.head)

	val listC = listB ++ new Cons(5, new Cons(1, new Cons(9, Nil)))
	val deepList = new Cons(listB, new Cons(listA, new Cons(listC, Nil)))

	val listD = List.flatten(deepList)
	val predicate: Predicate[Int] = new Predicate[Int] {
		override def apply(elem: Int): Boolean = elem % 2 == 0
	}

	println(listD filter predicate)  //output: [2 4 2 4]
}

//covariant singly-linked list trait
trait List[+T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
	def add[S >: T](elem: S): List[S]
	def ++[S >: T](other: List[S]): List[S]
	def reverse: List[T]
	def filter[S >: T](predicate: Predicate[S]): List[S]
}

//Case 1: singleton object extending List[Nothing]
object Nil extends List[Nothing] {
	override def isEmpty: Boolean = true
	override def head: Nothing = throw new NoSuchElementException
	override def tail: List[Nothing] = throw new UnsupportedOperationException
	override def add[S >: Nothing](elem: S): List[S] = new Cons(elem, Nil)
	override def ++[S >: Nothing](other: List[S]): List[S] = other
	override def reverse: List[Nothing] = Nil
	override def toString: String = "[]"
	override def filter[S](predicate: Predicate[S]): List[Nothing] = Nil
}

//Case2: non-empty List implementation
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	override def isEmpty: Boolean = false
	override def add[S >: T](elem: S): List[S] = new Cons(elem, this)  //add new element to beginning of list
	override def ++[S >: T](other: List[S]): List[S] = new Cons(head, tail ++ other)  //infix stack-recursive call new Cons(head, tail.++(other)). This ends when head === Nil object and Nil.++(other) is called; this is essentially the base case

	override def reverse: List[T] = {
		@tailrec
		def reverseUtil(input: List[T], output: List[T]): List[T] = {
			if (input.isEmpty) output
			else reverseUtil(input.tail, new Cons(input.head, output))  //everything is done in the forward recursive iteration
		}

		reverseUtil(this, Nil)
	}

	override def toString: String = {
		def enumerateAll(list: List[T]): String = 
			if(list.isEmpty) ""
			else if (list.tail.isEmpty) "" + list.head
			else list.head + " " + enumerateAll(list.tail)
		
		"[" + enumerateAll(this) + "]"
	}

	override def filter[S >: T](predicate: Predicate[S]): List[S] = {  //returns new linked list with only elements that match predicate condition. Condition is user-defined and passed in as an object!
		if(predicate.apply(head)) new Cons(head, tail filter predicate)
		else tail filter predicate
	}
}

//singleton object that flattens a deep list, useful in functional programming and flatmap
//come back to!!!
object List {
	def flatten[T](deepList: List[List[T]]): List[T] = {
		if(deepList.isEmpty) Nil
		else deepList.head ++ flatten(deepList.tail)

		def flattenUtil(remaining: List[List[T]], currentListExpanding: List[T], acc: List[T]): List[T] = {
			if (currentListExpanding.isEmpty) {
				if (remaining.isEmpty) acc
				else flattenUtil(remaining.tail, remaining.head, acc)
			}
			else flattenUtil(remaining, currentListExpanding.tail, new Cons(currentListExpanding.head, acc))
		}

		flattenUtil(deepList, Nil, Nil).reverse
	}
}

//we want to be able to filter out elements in collections that confrom to a given predicate
//**** traits can be instantiated to implement the functions *******
trait Predicate[T] {
	def apply(elem: T): Boolean
}