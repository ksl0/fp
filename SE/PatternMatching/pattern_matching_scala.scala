/*package fpinscala.datastructures*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def sum(ints: List[Int]): Int = ints match {
case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
}
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

//Ex1 - 3
//Ex2

def tail[A](as: List[A]): List[A] = as match{
	case Nil => Nil
	case x::Nil => Nil
	case x::xs => x::tail(xs)
}

//ex3
def setHead[A](as: List[A], n: A): List[A] = as match{
  case Nil => Nil
	case x::xs => n::xs
}

//ex4
def drop[A](as: List[A], n: Int): List[A] ={
  if (n == 0) as
	else as match {
		case Nil => Nil 
		case x::xs => drop(xs,n-1)}
}

//ex5

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case Nil => Nil
	case x::xs => {if (f(x)) dropWhile(xs, f) else x::xs}
	
}

//ex6

def init[A](l: List[A]): List[A] = l match{
  case Nil => Nil
	case x::Nil => Nil
	case x::xs => x::init(xs)
}

//ex7 nope, it waits until it's final fold thing
//ex8 data constructors of List, equivalent to foldRight w/ Nil and Cons
/***********  FOLDS!!! **********************/
def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
  case Nil => z
  case x::xs => f(x, foldRight(xs, z)(f))
  }

	//ex9
def length[A](l: List[A]) = foldRight(l,0)((x,y) => 1+ y)

//ex10
def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B ={
  l match{
    case Nil => z
    case x::xs => foldLeft(xs,f(z,x))(f)
  }
}

//ex11
def sumL(l: List[Int]) = foldLeft(l,0)(_+_)
def productL(l: List[Int]) = foldLeft(l,0)(_*_)
def length(l: List[Int]) = foldLeft(l,0)((x,y) => x+1)

//ex12
def reverse(l: List[Int]): List[Int] = l match {
  case Nil => Nil
  case x::xs => reverse(xs) ++ List(x)
}

def reverse[A](l: List[A]): List[A] =
  foldLeft(l, List[A]())((x,y) => y::x)

//ex13 - optional
//ex14 - found odd solution

def append[A](a: List[A], b: List[A]) =
  foldLeft(a.reverse, b)((x,y) => y::x) //why doesn't x::y work?
  //x is type B, y::x is also type B but x::y is type A
def appendViaFoldRight[A](a: List[A], b: List[A]) =
  foldRight(a,b)(_::_)
  
//ex15 (hard) - nc, Nil: List[A]
//still need better understanding....
def concate[A](xss: List[List[A]]) = 
  foldRight(xss, Nil:List[A])(append)

 //ex16
def addOne(l: List[Int]): List[Int] = l match{
  case Nil => Nil
  case x::xs => List(x+1) ++ addOne(xs)
}  

def addOne(l: List[Int]): List[Int] =
foldRight(l, Nil:List[Int])((h,t) => ((h+1)::t))

//ex17
def toDoubs(l: List[Int]): List[Double] = l match{
  case Nil => Nil 
  case x::xs => List(x.toDouble)++toDoubs(xs)
}

def toDoubs(l: List[Int]): List[Double] = 
foldRight(l, Nil: List[Double])((h,t) => h.toDouble::t)

//ex18

def map[A,B](l: List[A])(f: A => B): List[B] =
foldRight(l, Nil: List[B])((h,t) => f(h)::t)

//ex19
def filter[A](l: List[A])(f: A => Boolean): List[A] =
foldRight(l, Nil: List[A])((h,t) => if (f(h)) h::t else t)
//odd numbers in List[Int]
// val l = List.range(1,21)
//def isEven(a: Int): Boolean = if (a%2 ==0) true else false
// filter(l)(isEven)
//*can also call with */
// l.filter(isEven)

//ex20
def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
foldRight(l, Nil: List[B])((h,t) => (f(h) ++ t))

//concise book answer
def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
concate(map(l)(f))

//ex21
def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
flatMap(l)(h => if (f(h)) List(h) else Nil)

//ex22 - yikes this is messy
def listCons(a: List[Int], b: List[Int]): List[Int] = a match {
  case Nil => b
  case x::xs => b match {
    case Nil => x::xs
	case y::ys => (x+y)::listCons(xs,ys)
  }
}

//textbooks simplified
//??? why return Nil when a or b list equal Nil? - error reporting?
def addPairs(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
  case (_, Nil) => Nil
  case (Nil, _) => Nil
  case (x::xs, y::ys) => ((x+y)::addPairs(xs,ys)) 
}

//ex23
//same as combo(l,l)(_+_)
def combo[A](a: List[A], b: List[A])(f: (A,A) => A): List[A] = (a, b) match {
  case (_, Nil) => Nil
  case (Nil, _) => Nil
  case (x::xs, y::ys) => (f(x,y)::combo(xs,ys)(f)) 
}

//ex24 - easy

def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l,sub) match {
  case (_,Nil) => true
  case (Nil, _) => false
  case (x::xs, y::ys) => if (x == y) hasSubsequence(xs, ys)
   else hasSubsequence(xs, sub)
}

///TREES!!! ///////////////////////////////////////////
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

//ex25

def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 0
  case Branch(l,r) => (1 + size(l) + size(r))
}

//ex26 - nc

def maximum(t: Tree[Int]): Int = t match{
  case Leaf(x) => x
  case Branch(l,r) => maximum(l) max maximum(r)
}

//ex27- close?
def depth[A](t: Tree[A]): Int = t match{
  case Leaf(_) => 0
  //case Branch(l,r) => (1+ depth(l)) max (1+ depth(r))
  case Branch(l,r) => 1+ (depth(l) max depth(r))
}

//ex28 - use map - restudy
def mapTree[A,B](t: Tree[A])(f: A => B): Tree[B] = t match{
  case Leaf(x) => f(x)
  case Branch(l,r) => Branch(map(l)(f), map(r)(l))
}

//ex29 - almost

def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match{
  //case Leaf(x) => b(l(x), l(x))
  case Leaf(a) => l(a)
  case Branch(lf, rt) => b(fold(lf)(l)(b), fold(rt)(l)(b))  
}

def size2[A](t: Tree[A]): Int =
  fold(t)(a => 1)(1 +_+_)

def depth2[A](t: Tree[A]): Int =
  fold(t)(a => 0)((x,y) => 1 + (d1 max d2))

def max2(t: Tree[Int]): Int =
  fold(t)(a => a)(_ max _)
  
def mapTree[A](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(x => Leaf(f(x): Tree[B])(Branch(_,_))
  

