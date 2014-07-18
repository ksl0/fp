//somehow doesn't compile... - actually does now 
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Option(f(a))
  } 
  
  def flatMap[B](f: A => Option[B]): Option[B] = 
    map(f) getOrElse(None)
  
 
  def getOrElse[B >: A](default: => B): B = this match{
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this  
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a))=> this 
    case _ => None 
  }
  def filterViaFlatMap(f: A => Boolean): Option[A] = 
    flatMap{a => if (f(a)) this else None}
}

//ex2 

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
  case Nil => z
  case x::xs => f(x, foldRight(xs, z)(f))
  }

def mean_1(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum/xs.length)
}

import math.pow

def variance(xs: Seq[Double]): Option[Double] = {
  val m = mean_1(xs)
 // m.flatMap(m => Option(foldRight(xs.toList, 0.0)((a,b) => math.pow(a-m, 2) + b)))
 mean_1(xs).flatMap (m => mean(xs.map(x => math.pow(x-m,2))))
}

//common pattern: transform an Option via calls to map, flatMap, and/or filter, then use getOrElse to do error handling at the end.
//useful Try function
def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }
  
//ex3 - map22 is from textbook
/*
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
  if ((a == None) || (b == None)) None
  else Some(f(a.get, b.get))
}
*/

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] ={
  a.flatMap(x => b.map(y => f(x,y)))
}

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] ={
  for{x <- a; y<-b} yield(f(x,y))
}

//ex4 -nc
def sequence[A](a: List[Option[A]]): Option[List[A]] = a match{
  case Nil => Option(Nil)
  case x::xs => x flatMap(h => sequence(xs) map (h::_))
}

//ex5 - nc

def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
a.foldRight[Option[List[B]]](Some(Nil))(((x,y) => map2(f(x),y)((a,b) => a::b)))
  
def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
a match{
  case Nil => Some(Nil)
  case x::xs => map2(f(x), traverse(xs)(f))(_::_)
}

def sequence2[A](a: List[Option[A]]): Option[List[A]] = 
traverse(a)(x => x map {x => x})


//ex6


sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match{
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match{
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
    case Right(a) => Right(a)
   case Left(_) => b
  }
  //? explain
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
    Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)
}
case class Left[+E](value: E) extends Either[E, Nothing] 
case class Right[+A](value: A) extends Either[Nothing, A] 

//ex7

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match{
  case Nil => (Nil)
  case x::xs => map2(f(x), traverse(xs)(f))(_::_)
}

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
traverse(es)(x => x map {x => x})

//ex8