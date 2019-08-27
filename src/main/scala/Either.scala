sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(value) =>  Right(f(value))
    case Left(value) => Left(value)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => f(value)
    case Left(value) => Left(value)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(value) => Right(value)
    case Left(value) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(aValue: A) => b match {
      case Right(bValue: B) => Right(f(aValue, bValue))
      case Left(bError: EE) => Left(bError)
    }
    case Left(value: E) => Left(value)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es match {
      case Nil => Right(Nil)
      case Cons(Right(head: A), tail: List[Either[E,A]]) => sequence(tail).map(a => Cons(head, a))
      case Cons(Left(e), tail) => Left(e)
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val listEithers: List[Either[E, B]] = List.map(as)(f)
    sequence(listEithers)
  }

}

object EitherMain extends App {
  import Either._

  // Sequence
  val testRightSequence = List(Right(1), Right(2), Right(3))
  println(sequence(testRightSequence))

  val testLeftSeq = List(Right(1), Left("ERR"), Right(3))
  println(sequence(testLeftSeq))

  val testEmpty = List()
  println(sequence(testEmpty))

  // Traverse
  val testList = List(2,4,6)
  println(traverse(testList)(i => if (i % 2 == 0) Right(i) else Left("Not Even")))
  println(traverse(List.map(testList)(_ + 1))(i => if (i % 2 == 0) Right(i) else Left("Not Even")))

}
