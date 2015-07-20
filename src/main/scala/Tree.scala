sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  val testTree = Branch(Branch(Branch(Leaf(3), Branch(Leaf(4),Leaf(5))),Leaf(900)),Branch(Leaf(10), Leaf(4)))

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

  def depthRecurse[A](tree: Tree[A]): Int = tree match {
    case Leaf(value: Int) => 0
    case Branch(left, right) => {
      if (1 + depthRecurse(left) > 1 + depthRecurse(right))  1 + depthRecurse(left) else 1 + depthRecurse(right)
    }
  }

  // Exercise 3.28
  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value: A) => Leaf(f(value))
      case Branch(left, right) => Branch(mapTree(left)(f), mapTree(right)(f))
    }
  }

  // Exercise 3.29
  // Implement Fold and re-implement tree functions
  def fold[A,B](tree: Tree[A])(baseCaseFunction: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(value: A) => baseCaseFunction(value)
    case Branch(left, right) => f(fold(left)(baseCaseFunction)(f), fold(right)(baseCaseFunction)(f))
  }

  def sizeWithFold(tree: Tree[Int]):Int = {
    fold(tree)((item: Int) => 1)((left: Int, right: Int) => left + right + 1)
  }
  
  def maxWithFold(tree: Tree[Int]):Int = {
    fold(tree)((item: Int) => item)((leftThing: Int, rightThing: Int) => if (leftThing > rightThing) leftThing else rightThing )
  }

  def depthWithFold(tree: Tree[Int]): Int = {
    fold(tree)((item: Int) => 0)((left: Int, right: Int)=> if (left > right) left + 1 else right + 1)
  }

  // Note: you need some way to tell type system what the relationship between branch/leaf types are, one is shown below,
  // the other is to say that the leaf is of type Tree[B], like so: Leaf(f(leafItem)):Tree[B]
  def mapWithFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](tree)((leafItem: A) => Leaf(f(leafItem)))((left, right) => Branch(left, right))
  }



}
