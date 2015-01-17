def many[A](p: Parser[A]): Parser[List[A]]
def map[A,B](p: Parser[A]): Parser[B]
def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
//returns list of the elements

def map2_[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = 
  p.map(a => p2.map(b => f(a,b))

)


def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = 
  map(product(p, p2))(f.tupled)

def succeed[A](a: A): Parser[A] =
  string("") map (_ => a)

def many1[A](p: Parser[A]): Parser[List[A]] = 
  map2(p, many(p))(_ :: _)
//so many1 just appends the parser to the actual thing? 
//p followed by many(p)
//"want to recognize one or more 'a' characters"

/*ex. 2 Laws to specify the behavior of product */
/*
* order does matter for tuple; parser(a,b) != parser(b,a)
* exits if the types p, p2 are not of Parser[A] or Parser[B]
* inserts them into a tuple
* product is associative, (a,b), c == a, (b, c) 
* map and product can be used in whatever order to have end product 
*/

//define in terms of or, map2, succeed - nc
def many[A](p: Parser[A]): Parser[List[A]] =
  map2(p, many(p))(_::_) or succeed(List()) 
