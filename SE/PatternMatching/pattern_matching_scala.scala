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
