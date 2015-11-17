
trait Par[A] {

  def sum(ints: IndexedSeq[Int]):Int = {
    if (ints.size <=1) ints.headOption.getOrElse(0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      Par.get(sumL) + Par.get(sumR)
    }
  }

  def parSum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
      // this is something concrete, 1 or no numbers in list. No need to fork a new thread. No calculation
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(parSum(l), parSum(r))(_ + _)
    }

  def parSumWithFork(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    // this is something concrete, 1 or no numbers in list. No need to fork a new thread. No calculation
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      // here we don't know if the sum is super long, so we want to parallelize
      Par.map2(Par.fork(parSumWithFork(l)), Par.fork(parSumWithFork(r)))(_ + _)
    }
  // This builds up a big thunk chain. functions waiting for functions.


}

// We create a singleton Par? Companion object to the Par trait? Should the Par trait be covariant?
object Par {
  def unit[A](a: A): Par[A] = ???
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def get[A](a: Par[A]): A = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  // Exercise 7.1
  def map2[A, B, C](b: Par[A], b2: Par[B])(f: (A, B) => C):Par[C] = ???

}

