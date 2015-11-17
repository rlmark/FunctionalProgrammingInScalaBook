package Stream
import Stream.cons


sealed trait Stream[+A] {

  def headOption:Option[A] = {
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
  }

  // Exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // Exercise 5.2
  // implement take, which returns the first n elements of a Stream,
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n == 0 => Empty
    case Cons(h, t) => cons(h(), t().take(n-1))
    //case Cons(head, tail) => Cons(head, () => tail().take(n-1))
  }

  // implement drop, which returns the stream minus the first n elements,
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) if n == 1 => tail()
    case Cons(head, tail) => tail().drop(n-1)
  }

  // Exercise 5.3
  // return all starting elements of a Stream that match the given predicate.
  // Note: I think I accidentally wrote Filter, not takeWhile. Ask Kate.
  def filterStream(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => {
      if (f(head())) Cons(head, () => tail().filterStream(f))
      else tail().filterStream(f)}
  }

  // HAHAHA this is THE REAL TAKEWHILE...
  def takeWhile2(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(head, tail) => {
      if (f(head())) cons(head(), tail().takeWhile2(f))
      else Empty
    }
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
  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(head, tail) => p(head()) && tail().forAll(p) //if (!p(head())) false else tail().forAll(p)
  }
//  def forAllFold(p: A => Boolean): Boolean = {
//   foldRightStream(false)((a, b) => p(a) || b)//if (!p(a)) b else true)
//  }

  // Exercise 5.5
  // return all starting elements of a Stream that match the given predicate.
  def takeWhileFold(f: A => Boolean): Stream[A] = {
    foldRightStream(Empty: Stream[A])((a, acc) => if (f(a)) Cons(() => a, () => acc) else Empty )
  }

  // Exercise 5.7
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
  // Why the difference in .this? 
  def filter(f: A => Boolean): Stream[A] = {
    this.foldRightStream(Empty: Stream[A])((a, acc) => if (f(a)) Cons(() => a, () => acc) else acc )
  }
  def filterFold(f: A => Boolean): Stream[A] = {
    foldRightStream(Empty: Stream[A])((a, acc) => if (f(a)) cons(a, acc) else acc)
  }

  // Exercise 5.13
  def mapUnfold[B](f: A => B):Stream[B] = {
    Stream.unfold(this: Stream[A]){s => s match {
      case Empty => None
      case Cons(a, tail) => Some((f(a()), tail()))
      }
    }
  }

  def takeUnfold(num: Int):Stream[A] = {
    Stream.unfold(num, this){
      case(1, Cons(h, t)) => Some(h(), (1,Empty))
      case(num, Cons(h, t)) => Some(h(), (num-1, t()))
      case _ => None
    }
  }


  def takeWhileUnfold(f: A => Boolean):Stream[A] = {
    Stream.unfold(this, f){
      case(Cons(h,t), f) => if (f(h()) == true) Some(h(), (t(), f))
                            else None
      case(Empty, f) => None
    }
  }

  // zipWith takes two streams of A and a function for combining them to produce a new Stream
  def zipWithUnfold[B, C](stream2: Stream[B])(f: (A, B) => C):Stream[C] = {
    Stream.unfold(this, stream2, f){
      case(Cons(h1, t1), Cons(h2, t2), f) => Some( f(h1(), h2()), (t1(), t2(), f) )
      case _ => None
    }
  }

//  // zipAll function should continue the traversal as long as either stream has more elements
//  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
//    Stream.unfold((this, s2)){
//      Go through each case, if there are both streams, one, the other, or none, and Cons them into a new stream???
//      case (Cons(h1, t1), Cons(h2, t2)) => Cons( )
//  }

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

  // Exercise 5.9
  // generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on
  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  // Exercise 5.10
  def fibs:Stream[Int] = {
    cons(0, cons(1, increment(0,1)))
  }
  def increment(a: Int, b: Int):Stream[Int] = {
    cons(a+b, increment(b, a+b))
  }

  // Exercise 5.11
  // It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some((a: A, s: S)) => cons(a, unfold(s)(f))
  }


  // Exercise 5.12
  // Write fibs, from, constant, and ones in terms of unfold.
  def onesUnfold:Stream[Int] = unfold(1)(self => Some((self, 1)))
  def constantUnfold[A](i: A): Stream[A] = unfold(i)(self => Some((self, i)))
  def fromUnfold(n: Int):Stream[Int] = unfold(n)(number => Some((number, number+1)))
  def fibsUnfold:Stream[Int] = {
    unfold((0, 1))((tuple) => Some((tuple._1, (tuple._2, tuple._1 + tuple._2))))
  }
  def fibsUnfold2: Stream[Int] = {unfold((0,1)){case (s1, s2) => Some(s1, (s2,s1 + s2))}}
}

