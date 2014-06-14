//CHPT 2 Exercises #1 - 5
object HelloWorld {
    def main(args: Array[String]) {
      println("Hello, world! ")
    }
  }
  HelloWorld.main(null)
  
	//Exercise 1
	//recusive fibonacci sequence
def fib(n: Int): Int ={
  
	def go(n: Int, a: Int, b: Int): Int ={
	  if (n == 1) a
		else go(n-1, b, a+b)
	}
 go(n, 1, 1)
}

def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean ={
  def loop(n: Int): Boolean = {
	 if (n +1 >= as.length) true
	 else if (gt(as(n), as(n+1))) loop(n+1)
	 else false
 }
 loop(0)
}

def gt(a:Int,b:Int): Boolean = if (a > b) true else false

//isSorted(Array(3,2,1), gt) returns true

def curry[A,B,C](f: (A, B) => C): A => (B => C) = 
  (a:A) => (b:B) => f(a,b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
((a:A), (b:B)) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C =
(a:A) => f(g(a))

####