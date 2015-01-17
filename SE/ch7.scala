//Chp7

//wrong: did not include that a, b were type Par
//def map2[A,B,C](a: A, b: B)(f: (A,B) => C): Par[C]

//par is a monad..
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit} 

import java.util.concurrent._

type Par[A] = ExecutorService => Future[A]
def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

object Par {
def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

private case class UnitFuture[A](get: A) extends Future[A] {
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}

def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
= (es: ExecutorService) => {
  val af = a(es)
  val bf = b(es)
  UnitFuture(f(af.get, bf.get)) 
}

def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

def fork[A](a: => Par[A]): Par[A] =  
  es => es.submit(new Callable[A] {
  def call = a(es).get
})


// ex 4
//def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

def asyncF[A,B](f: A => B): A => Par[B] = 
  a => fork(unit(f(a)))
def map[A,B](pa: Par[A])(f: A => B): Par[B] =
  map2(pa, unit(()))((a,_) => f(a))

  //ex5 - nc
def sequence[A](l: List[Par[A]]): Par[List[A]] =
  l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_::_))
  //ex6 - nc 
 
def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
  val pl = l map(asyncF(a => if (f(a)) List(a) else List()))
  map(sequence(pl))(_.flatten)
}

//def map3[A,B,C,D]() =???
//split left right
// then put into sequence


def sum(ints: IndexedSeq[Int]): Int =
  if (ints.size <= 1)
    ints.headOption getOrElse 0
  else {
    val (l,r) = ints.splitAt(ints.length/2)
    sum(l) + sum(r)
  }

}
