import List._
import Monoid.Monoid
import Option._
import Stream.Stream._
import Tree._

import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {

    //Exercise 2.1
    def fibonacciNonTailRec(n: Int): Int= {
      if (n == 1) 0
      else if  (n == 2) 1
      else fibonacciNonTailRec(n - 1) + fibonacciNonTailRec(n - 2)
    }
    println(fibonacciNonTailRec(12))

    def fibonacciTailRec(n: Int): Int = {
      @tailrec
      def loop(currentN: Int, a: Int, b: Int): Int = {
        if (currentN == n) b
        else loop(currentN + 1, b, a + b)
      }
      loop(1, 1, 0)
    }
    println(fibonacciTailRec(12))

    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
      def loop(head: A, tail: Array[A]): Boolean = {
        if (head == Nil) true
        else if (!ordered(head, tail.head)) false
        else loop(tail.head, tail.tail)
      }
      loop(as.head, as.tail)
    }

    // Exercise 2.3
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
      (a: A) => (b: B) => f(a,b)
    }

    // Exercise 2.4
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }

    // Exercise 2.5
    def compose[A,B,C](f: B => C, g: A => B): A => C = {
      (a: A) => f(g(a))
    }

    // Exercise 2.6
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    // answer is 3.

    // Exercise 3.2 see List object.
    println(tail(List(1,2,3,4)))

    // Exercise 3.3
    println(setHead(41,List(2,3,4,5,6)))

    // Exercise 3.4
    println(drop1(List(1,2,3,4,5,6,7),5))
    println(drop(List(1,2,3,4,5,6,7),5))
    println(drop(List(1,2),4))

    // Exercise 3.5
    def f(i: Int): Boolean = {
      i == 5
    }
    def y(i: Int): Boolean = {
      i < 5
    }
    println(dropWhile(List(2,3,4,5,6), f))
    println(dropWhile(List(2,3,4,5,6), y))

    // Exercise 3.6
    println(init(List(1,2,3,4,5)))
    println(init(List(1)))
    println(init(List()))

    // Exercise 3.8
    println(foldRight(List(1,2,3,4,5), Nil:List[Int])(Cons(_,_)))
    // What do you think this says about the relationship between foldRight and the data constructors of List?
    // FoldRight can be used to construct lists? When you pass Nil and Cons into FoldLeft you end up with Reverse.

    // Exercise 3.9
    println(length(List(1,2,3,4,5)))
    println(length(List()))

    // Exercise 3.11
    println(sumLeft(List(1,2,3)))

    // Exercise 3.12
    println(reverse(List(1,2,3)))

    // Exercise 3.14
    // Implement append using foldLeft or foldRight
    println(append(List(1,2,3), List(10,11,12)))
//    println(append2(List(1,2,3), List(10,11,12)))
    println(append3(List(1,2,3), List(10,11,12)))
    println(append4(List(1,2,3), List(10,11,12)))
    println(append5(List(1,2,3), List(10,11,12)))

    // Exercise 3.15
    println(concatenateLists(List(List(100,101,102), List(201,202,203), List(301,302,303))))

    // Exercise 3.16
    println(add1(List(5,6,7,8)))

    // Exercise 3.17
    println(doubleToString(List(0.0, 2.0, 4.0523423423, 6.0, 8.0)))
    println(doubleToStringFold(List(0.0, 2.0, 4.0523423423, 6.0, 8.0)))

    // Exercise 3.18
    def a(i: Int): Int = {
      i + 5
    }
    println(map(List(10,11,12,13))(a))

    // Exercise 3.19
    def even(a: Int):Boolean = {
      if (a % 2 == 0) true
      else false
    }
    println(filter(List(1,2,3,4,5,6,7,8))(x => even(x)))

    // Exercise 3.20
    println(flatMap(List(1,2,3,4))(i => List(i,i)))

    // Exercise 3.21 THIS ONE IS BROKEN
    println(filterFlatMap2(List(1,2,3,4,5,6,7,8))(even))

    // Exercise 3.22
    println(correspond(List(1,2,3,4), List(4,5,6,7)))

    // Exercise 3.23
    println(zipWith(List(1,2,3,4), List(4,5,6,7)){(x, y) => x + y})

    // Exercise 3.24
    val superList = List(1,2,5,3,4)
    val subList = List(1,2,3)
    println(hasSubsequence(superList, subList))

    // Exercise 3.25
    println(size(testTree))

    // Exercise 3.26
    println(max(testTree))

    // Exercise 3.27
    println(depth(testTree))

    // Exercise 3.28
    println(mapTree(testTree)(x => x.toString + "!"))

    // Exercise 3.29
    println(fold(testTree)((a: Int) => a)((x: Int,y: Int) => x + y))

    println(sizeWithFold(testTree))
    println(maxWithFold(testTree))
    println(depthWithFold(testTree))
    println(depthRecurse(testTree))

    // Exercise 4.1
    println(Some(5).map(i => i + 1))
    println(Some(5).flatMap(i => Some(i + 1)))

    println(Some(5).getOrElse("I'm missing a number"))
    println(None.getOrElse("I'm missing a number"))

    println(Some(12).orElse(Some("ReplacementOption")))
    println(None.orElse(Some("ReplacementOption")))

    println(Some(14).filter(even))
    println(Some(13).filter(even))

    // Exercise 4.2
    // Why????? Why does variance need to be called on an option, even a nonsensical one
    println(Some(Nil).variance(Seq(0.0,103.4,0.04,1.403,93.92)))

    // Exercise 4.3
    println(map2(Some(3), Some(4))((x,y) => x + y))
    println(map2(None, Some(4))((x: Int,y: Int) => x + y))

    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), None, Some(3))))

    // ~~~~ STREAM EXERCISES ~~~~ //
    println(testStream.headOption)

    // Exercise 5.1
    println(testStream.toList)

    // Exercise 5.2
    println(testStream.take(3).toList)
    println(testStream.drop(5).toList)
    println(testStream.drop(2).toList)

    // Exercise 5.3
    def lessThanFour(i: Int): Boolean = {
      i <= 3
    }
    println(testStream.filterStream(lessThanFour).toList)
    println(testStream.takeWhile2(lessThanFour).toList)

    // Exercise 5.4
    println(testStream.forAll(lessThanFour))

    // Exercise 5.5
    println(testStream.takeWhileFold(lessThanFour).toList)
    println("STILL Working?")
    println(testStream.filterFold(lessThanFour).toList)

    // Exercise 5.6
    println(testStream.map(x => x.toString + "hi").toList)
    println(testStream.appendStream(testStream).toList)
    println(testStream.filter(x => x%2 == 1).toList)

    // Exercise 5.7
    println(ones.take(4).toList)

    // Exercise 5.8
    println(constant(5).take(6).toList)

    // Exercise 5.9
    println(from(4).take(5).toList)

    // Exercise 5.10
    println(fibs.take(7).toList)

    // Exercise 5.12
    println(onesUnfold.take(6).toList)
    println(constantUnfold("a").take(4).toList)
    println(fromUnfold(6).take(10).toList)
    println(fibsUnfold.take(10).toList)

    // Exercise 5.13
    println(testStream.mapUnfold(x=> x.toString).toList)
    println(from(1).mapUnfold(x=> x * 2).take(10).toList)

    println(testStream.takeUnfold(12).toList)
    println(testStream.takeUnfold(1).toList)

    println(testStream.takeWhileUnfold(x => x < 5).toList)
    println(testStream.takeWhileUnfold(x => x < 1000).toList)


    println(testStream.zipWithUnfold(testStream.take(3))((x,y) => x + y).toList)
    println(testStream.zipWithUnfold(testStream)((x,y) => x + y).toList)

    def addBits(a: Int, b: Int) :Int = {
      if (b == 0) a
      else {
        val sum = a ^ b //SUM of two integer is A XOR B
        val carry = (a & b) << 1  //CARRY of two integer is A AND B
        addBits(sum, carry)
      }
    }

    println(addBits(4,5))
    println(addBits(7,1))
    println(addBits(6,3))

    def subtractBits(a: Int, b: Int) :Int = {
      addBits(a,~b) + 1
    }

    println(subtractBits(5,4))
    println(subtractBits(7,4))
    println(subtractBits(10,4))
    println(subtractBits(12,1))
    println(subtractBits(1, 12))
    println(subtractBits(0,0)) // weird case
    println(subtractBits(-1,-2))

    println("HI THIS IS WHERE YOU ARE")

    val seq = IndexedSeq(1,2,3,4)
    val additionMonoid: Monoid[String] = Monoid.stringMonoid
    println(Monoid.foldMapV(seq, additionMonoid)(i => i.toString))
    println(Monoid.foldMapV(IndexedSeq(1), additionMonoid)(i => i.toString))
    println(Monoid.foldMapV(IndexedSeq(), additionMonoid)(i => i.toString))


  }
}
