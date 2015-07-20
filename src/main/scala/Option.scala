import scala.{Option => _, Some => _, Either => _, None => _}

sealed trait Option[+A] {

  // Exercise 4.1
  // Implement the following methods for Option class
  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case value @ None => value
  }
  // note: this last case is a way of passing along the value from original thing being pattern matched upon (the scrutinee)

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }

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

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
object Option






