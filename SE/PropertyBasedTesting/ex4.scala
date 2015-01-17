case class Gen[A](sample: State[RNG, A])
def choose(start: Int, stopExclusive: Int): Gen[Int] = 
   Gen(State(r => double(r)*(stopExclusive - start) + start))

