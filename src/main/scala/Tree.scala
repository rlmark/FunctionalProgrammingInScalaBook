sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  val testTree = Branch(Branch(Branch(Leaf(3), Branch(Leaf(4),Leaf(5))),Leaf(900)),Branch(Leaf(10000), Leaf(4)))

  // Exercise 3.25
  // write a function, size, which counts number of nodes and branches in a tree
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) =>  1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3.26
  // write a function, max, which returns the maximum value of the tree
  def max(tree: Tree[Int]):Int = {
    def counter(tree: Tree[Int], currentMax: Int): Int = tree match {
      case Leaf(value) => if (value > currentMax) value else currentMax
      case Branch(left, right) => if (counter(left, currentMax) > counter(right, currentMax)) counter(left,currentMax) else counter(right,currentMax)
    }
    counter(tree, 0)
  }

  // Exercise 3.27
  def depth[Int](tree: Tree[Int]): Integer = {
    def counter(tree: Tree[Int], count: Integer): Integer =  tree match {
      case Leaf(value: Integer) => count
      case Branch(left, right) => {
        val newCount = count + 1
        if (counter(left, newCount) > counter(right, newCount)) counter(left, newCount) else counter(right, newCount)
      }
    }
    counter(tree, 0)
  }

  // Exercise 3.28
  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value: A) => Leaf(f(value))
      case Branch(left, right) => Branch(mapTree(left)(f), mapTree(right)(f))
    }
  }

  // Exercise 3.29
  // Implement Fold
//  def fold[A, B](tree: Tree[A])(f: (A, B) => B)(doSomethingToLandR: ((Tree[A], Tree[A]) => B)): B = {
//    tree match {
//      case Leaf(value: A) => f(value)
//      case Branch(left, right) => doSomethingToLandR( fold(left)(f)(doSomethingToLandR), fold(right)(f)(doSomethingToLandR) )
//    }
//  }

    def fold1[A, B](tree: Tree[A])(f: A => B): B = ???

//  def foldIdentity(tree: Tree[Int]): Tree[Int] = {
//    fold(tree)(x => x)((l,r) => Branch(foldIdentity(l, r)))
//  }
}
