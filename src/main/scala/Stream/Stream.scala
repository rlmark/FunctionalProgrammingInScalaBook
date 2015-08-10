package Stream

import scala.collection.immutable.Stream.cons

sealed trait Stream[+A] {

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h , t) => h() :: t().toList
  }

  // Exercise 5.2
  // implement take, which returns the first n elements of a Stream,
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n == 0 => Empty
    case Cons(head, tail) => Cons(head, () => tail().take(n-1))
  }

  // implement drop, which returns the stream minus the first n elements,
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) if n == 1 => tail()
    case Cons(head, tail) => tail().drop(n-1)
  }

  // Exercise 5.3
  // return all starting elements of a Stream that match the given predicate.
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => {
      if (f(head())) Cons(head, () => tail().takeWhile(f))
      else tail().takeWhile(f)}
  }

  // the second argument here is lazy. So I can short circuit, whereas with regular list it will not short circuit.
  def foldRightStream[B](x: => B)(f: (A, => B) => B): B = this match {
    case Empty => x
    case Cons(head, tail) => f(head(), tail().foldRightStream(x)(f))
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists2(p: A => Boolean): Boolean = foldRightStream(false)((a, b) => p(a) || b)

  // Exercise 5.4
  // Checks that all elements in the Stream match a given predicate.
  // Your implementation should terminate the traversal as soon as it encounters a non-matching value.
  def forAll(p: A=> Boolean): Boolean = this match {
    case Empty => true
    case Cons(head, tail) => p(head()) && tail().forAll(p) //if (!p(head())) false else tail().forAll(p)
  }
//  def forAllFold(p: A => Boolean): Boolean = {
//   foldRightStream(false)((a, b) => p(a) || b)//if (!p(a)) b else true)
//  }

  // Exercise 5.5
  // return all starting elements of a Stream that match the given predicate.
  def takeWhileFold(f: A => Boolean): Stream[A] = {
    foldRightStream(Empty: Stream[A])((a, acc) => if (f(a)) Cons(() => a, () => acc) else acc )
  }

  // Exercise 5.6
  // Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
  def map[B](f: A => B): Stream[B] = {
    foldRightStream(Empty: Stream[B])((a, acc) => Cons(() => f(a),() => acc))
  }

  // A is the existing Stream, and B can be an A or a Supertype of A. Greater than is Supertype
  // Scala type system might just infer the common parent type.
  def appendStream[B>:A]( a2: Stream[B]): Stream[B] = {
    foldRightStream(a2)((h, t) => Cons(() => h, () => t))
  }

  // Figure out type mismatch
  def flatMap[B](f: A => Stream[B]):Stream[B] = {
    def f2 (a: A, acc: => Stream[B]):Stream[B] = f(a).appendStream(acc) // Note: problem might have been with anonymous function syntax
    foldRightStream(Empty: Stream[B])(f2)
  }

  // Filter takes elements out of a Stream if they do not fulfill a predicate
  // Ask Jeremy, are these instance methods or class methods? They are called differently .notation vs (in params)
  def filter(f: A => Boolean): Stream[A] = {
    this.foldRightStream(Empty: Stream[A])((a, acc) => if (f(a)) Cons(() => a, () => acc) else acc )
  }





}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  val testStream = cons(1,cons(2,cons(8,cons(3,cons(9, empty)))))

  // These are not instance methods. Standalone functions. These are analogous to the class methods in ruby, which
  // standalone and build up objects
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8
  def constant[A](x: A): Stream[A] = {
    cons(x, constant(x))
  }

  // Exercise 5.11
  // It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
