sealed trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z }
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case None => Stream()
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
  def toList: List[A] = { //prevent stackoverflow
    @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h,t) => go(t(), h() :: acc)
        case _ => acc
      }
    go(this, List()).reverse
  }
  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a,b) => p(a) && b) 
  def append[B>:A](b: Stream[B]): Stream[B] =
    foldRight(this)((x,y) => Stream.cons(x,y))
  def drop(n: Int): Stream[A] = {
    def go(n: Int, as: Stream[A]): Stream[A] ={
      if (n > 0) this match { //if use "{case ...}" without "this match", annotates type as Stream[Any]
        case Cons(h,t) => go(n-1, t())
        case _ => Stream.empty //better to use case _ than case Empty
      } 
      else as
    }
    go(n, this)
  } 
  def takeWhile(p: A => Boolean): Stream[A] = {
      def go(p: A => Boolean, as: Stream[A]): Stream[A] ={
        this match {
          case Cons(h,t) => if (p(h())) Stream.cons(h(), go(p, t())) else Stream()
          case _ => Stream()
      }
    }
    go(p, this)
  }
  
  def zipWith[B](s2: Stream[B])(f: (A,B) => B): Stream[B] = 
  unfold((this, s2)){
    case (Cons(h,t), Cons(h1,t1)) => Some(((f(h(),h1())), (t(), t1())))
    case _ => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
    unfold(this, s2){
      case (Cons(h,t), Cons(h1,t1)) => Some(((Option(h()),Option(h1())), (t(), t1() )))
      case (Cons(h,t), Empty) => Some(((Option(h()), None), (t(), Stream())))
      case (Empty, Cons(h,t)) => Some(((None, Option(h())), (Stream(), t())))
      case _ => None
    }
  def startsWith[A](s: Stream[A]): Boolean = 
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  def tails: Stream[Stream[A]] = 
  unfold(this){
    case Empty => None
    case s => Some((s, s drop 1))
  } append (Stream(Stream.empty))

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
  
  
//answers
def startsWith[A](s: Stream[A]): Boolean = 
  zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (h,h2) => h == h2
  }
  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.

  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A,=>B) => B): Stream[B] = 
    foldRight((z, Stream(z)))((a,p) => {
      val b2 = f(a,p._1)
      (b2, cons(b2,p._2))
    })._2
//ex1
//Note: smart constructors lowercase of class, memoize + cache values. 
//best to infer type Stream[A] instead of Empty or Cons
def toList: List[A] = this match {
 case Cons(x,y) => x() :: y().toList
 case _ => List()
 
//ex2
def take(n: Int): Stream[A] = 
  if (n > 0) this match {
    case Cons(h, t) => Stream.cons(h(), t().take(n-1))
    case _ => Stream.empty // we can say Stream.empty
  }
  else Stream() 

def drop(n: Int): Stream[A] = {
  def go(n: Int, as: Stream[A]): Stream[A] ={
    if (n > 0) this match { //if use "{case ...}" without "this match", annotates type as Stream[Any]
      case Cons(h,t) => go(n-1, t())
      case _ => Stream.empty //better to use case _ than case Empty
    } 
    else as
  }
  go(n, this)
} 

//ex3 - stackoverflow - why? -- used non-tail recursive form of toList?

def takeWhile_0(p: A => Boolean): Stream[A] = {
    def go(p: A => Boolean, as: Stream[A]): Stream[A] ={
      this match {
        case Cons(h,t) => if (p(h())) Stream.cons(h(), go(p, t())) else Stream()
        case _ => Stream()
    }
  }
  go(p, this)
}

def takeWhile(p: A => Boolean): Stream[A] = this match {
  case Cons(h,t) if (p(h())) => Stream.cons(h(), t().takeWhile(p))
  case _ => Stream()
}

//ex4
def forAll(p: A => Boolean): Boolean = 
  foldRight(true)((a,b) => p(a) && b) //this works but I don't get foldRight that well

//ex5 - nc
def takeWhile2(p: A => Boolean): Stream[A] =
//  foldRight(Stream.empty)((a,b) => if (p(a)) cons(a, b.takeWhile2) else Stream())
//don't use recursion with takeWhile too
foldRight(Stream.empty[A])((a,b) => if (p(a)) Stream.cons(a, b) else Stream())

//ex6 

def headOption2: Option[A] = 
  foldRight(None: Option[A])((a,b) => if (Option(a) != None) Some(a) else None) 
  //makes more sense
  
def headOption_1: Option[A] =
foldRight(None: Option[A])((h,_) => Some(h)) 
//since foldRight returns (z) when does not match Cons(h,t), use nicer syntax :)

//ex7 - append nc, review types B >: A
def foldRight[B](z: => B)(f: (A, => B) => B): B =
  this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z }
    
def map[B](f: A => B): Stream[B] =
  foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))

def filter[B](f: A => Boolean): Stream[A] =
  foldRight(Stream.empty[A])((a,b) => if (f(a)) Stream.cons(f(a), b) else b)
  
/*  { 
  Stream(1,2,3).map(x => x+1).toList
  cons(2, Stream(2,3).map(x => x+1).toList
  Stream(2, cons(3, Stream(3).map(x => x+1).toList
  2::Stream(3, cons(4)).toList
  2::3::Stream(4, Stream().map(_+1).toList
  Stream(2, 3, 4)  
}*/ //function trace attempt?

def append[B>:A](b: Stream[B]): Stream[B] =
  foldRight(a)((x,y) => Stream.cons(x,y))
  /* why does the book have the function as 
  (s: => Stream[B])? Unit to Stream[B]? 
  */
def flatMap[B](f: A => Stream[B]): Stream[B]=
  foldRight(Stream.empty[B])((a,b) => append(f(a), b))

//ex8
def constant_0[A](a: A): Stream[A] = Stream.cons(a, constant(a))
def constant[A](a: A): Stream[A] = {
  lazy val tail: Stream[A] = Cons(() => a, () => tail)
  tail
}

//ex9 - hey lol I got this right!!
def from_0(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

//ex10
def fibs(n: Int, n1: Int): Stream[Int] = Stream.cons(n, fibs(n1, n1 + n)) 
//use fibs(1,1)

val fibs2: Stream[Int] ={
  def go(n: Int, n1: Int): Stream[Int] = 
    Stream.cons(n, go(n1, n1 + n)) 
  go(1,1)
}

//ex 11 
def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
  f(z) match {
    case None => Stream()
    case Some((a,s)) => Stream.cons(a, unfold(s)(f))
  }

//ex12 fibs3 nc...
def fibs3: Stream[Int] = unfold((0,1)){case (a,b) => Some((a),(b, a+b))}
def ones: Stream[Int] = unfold((1))(x => Some((x,x)))
def constant(n: Int): Stream[Int] = unfold(n)(x => Some((x,x)))
def from(n: Int): Stream[Int] = unfold(n)(x => Some((x, x+1)))

//ex13: map3 nc
def map3[B](f: A => B): Stream[B] = 
  unfold(this) {
    case Cons(h,t) => Some(f(h()), t())
    case _ => None
  }
  //unfold(this){case Cons(h,t) => Some(f(h()), t())}
  // - need to add other case to make it Option instead of Some
  
def take(n: Int): Stream[A] =
  unfold((this, n)) {
    //unfold(this) {
      //case Cons(h,t) if (n>0) => Some((h(), t()))
    case (Cons(h,t),n) if (n>0) => Some((h(), (t(), n-1)))
    case _ => None
  }
}

def takeWhile(f: A => Boolean): Stream[A]  = 
  unfold(this){
    case Cons(h,t) if (f(h)) => Some((h(t), t()))
    case _ => None
  }

def zipWith[B](s2: Stream[B])(f: (A,B) => B): Stream[B] = 
unfold((this, s2)){
  case (Cons(h,t), Cons(h1,t1)) => Some(((f(h(),h1())), (t(), t1())))
  case _ => None
}
def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = 
  unfold(this, s2){
    case (Cons(h,t), Cons(h1,t1)) => Some(((Option(h()),Option(h1())), (t(), t1() )))
    case (Cons(h,t), Empty) => Some(((Option(h()), None), (t(), Stream())))
    case (Empty, Cons(h,t)) => Some(((None, Option(h())), (Stream(), t())))
    case _ => None
  }

//ex14 -nc
def startsWith[A](s: Stream[A]): Boolean = 
  zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    case (h,h2) => h == h2
  }

//ex 15 
def tails: Stream[Stream[A]] = 
unfold(this){
  case Empty => None
  case s => Some((s, s drop 1))
} append(Stream(empty))

