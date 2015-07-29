import scala.{Option => _, Some => _, Either => _, None => _}
import List._

sealed trait Option[+A] {

  // Exercise 4.1
  // Implement the following methods for Option class
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case value @ None => value
  }
  // note: this last case is a way of passing along the value from original thing being pattern matched upon (the scrutinee)

  def map1[B](f: A => B): Option[B] = {
    this.flatMap(a => Some(f(a)))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }

  // so this.map(f) is an option of an option. then you unwrap the first layer of that option.
  def flatMap2[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def flatMap3[B](f: A => Option[B]): Option[B] = {
    flatten(map(f))
  }

  def flatten[A](option: Option[Option[A]]): Option[A] = {
    option.getOrElse(None)
  }

  // the arrow means lazy. Not evaluated until needed.
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case value @ Some(originalVal)  => value
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case value @ Some(a) => if (f(a)) value else None
    case None => None
  }

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    // First find the mean. m = mean of sequence xs
    mean(xs) flatMap { m =>
      val varianceSequence: Seq[Double] = xs.map(i => math.pow(i - m, 2))
      val varianceResult: Option[Double] = mean(varianceSequence)
      varianceResult
    }
  }

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

  // lift(f) returns a function which maps None to None and applies f to the contents of Some.
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f



}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  // Exercise 4.3
  // combines two Option values using a binary function. If either Option value is None, then the return value is too.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
    case Some(a) => b match {
      case Some(b) => Some(f(a, b))
      case None => None
    }
    case None => None
  }

  // Exercise 4.4
  // combines a list of Options into one Option containing a list of all the Some values in the original list.
  // If the original list contains None even once, the result of the function should be None

  //  def sequenceTradFold[A](as: List[Option[A]]): Option[List[A]] = {
  //    as.foldRight[Option[List[A]]](Some(Nil))((x ,y )=> map2(x,y)(_::_)) // first underscore is head, second is tail.
  //  }
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    foldRight(as, Some(List()):Option[List[A]]){ (optionA: Option[A], accumulator: Option[List[A]]) =>
      map2(optionA, accumulator)( (a: A, b: List[A]) => Cons(a, b))
      }
    }

  def sequence1[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case Cons(Some(head), tail) => sequence1(tail).map(Cons(head,_))
    case Cons(None, tail) => None
  }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val listOfOptionB = map(a)(i => f(i))
    sequence(listOfOptionB)
  }

//  def traverse1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
//    foldRight(as, Some(List()): Option[List[B]])((a: A, accumulator: Option[List[B]]) =>
//      map2(f(a), accumulator)() )
//  }

//  def traverse1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
//    as match {
//      case Nil => Some(Nil)
//      case Cons(a: A, tail: List[A]) => f(a) match {
//        case Some(b: B) => Some(b).map(Cons(b,traverse1(tail)(f))) // nope
//        case None => None
//      }
//    }
//  }

}






