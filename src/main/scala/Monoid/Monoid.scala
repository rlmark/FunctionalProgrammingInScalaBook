package Monoid

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero:A
}

object Monoid {
  // Are these anonymous classes?
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // EXERCISE 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 0
  }
  val intMultiplication: Monoid[Int]  = new Monoid[Int] {
    def op(a1: Int, a2: Int):Int = a1 * a2
    val zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero = true
  }

  // Exercise 10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse(a2)
    val zero = None
  }

  // Exercise 10.3
//  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
//    def op(a1: A => A, a2: A => A): Monoid[A => A] = a1.compose(a2)
//    val zero = (a:A) => a
//  }
//
//  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
//    def op(a1: A => A, a2: A => A): Monoid[A => A] = a1.andThen(a2)
//    val zero = (a:A) => a
//  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  // Exercise 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // Exercise 10.6
  def foldRightFromFoldMap[A,B](as: List[A], z: A)(f: A=> B): B = ???

  // EXERCISE 10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 0) {
      m.zero
    }
    else if (v.length == 1) {
      val a: A = v.head
      f(a)
    } else {
      // Dividing sequence in 2
      val floorMidPt = v.length / 2
      val twoSeq: (IndexedSeq[A], IndexedSeq[A]) = v.splitAt(floorMidPt)
      val (s1, s2) = twoSeq

      // Recursive call
      m.op(foldMapV(s1, m)(f), foldMapV(s2, m)(f))
    }
  }

}
