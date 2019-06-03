package xyz.ciroque.monadering.ChapterOne

trait Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def identity[A](tree: Tree[A]): Tree[A] = tree

  type WithCounter[A] = Int => (A, Int)
  type WithState[A, S] = S => (A, S)

  def relabel[A](tree: Tree[A]): WithCounter[Tree[(A, Int)]] = tree match {
    case Leaf(x) => i => (Leaf((x, i)), i + 1)
    case Node(left, right) => relabel(left) next { ll =>
      relabel(right) next { rr =>
        pure(Node(ll, rr))
      }
    }
  }

  def leafCount[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Node(left, right) => leafCount(left) + leafCount(right)
  }

  // Exercise 1.1
  //  def pure[A](x: A): WithCounter[A] = i => (x, i)
  def pure[A](x: A): WithState[A, Int] = s => (x, s)

  implicit class RichWithCounter[A](f: WithCounter[A]) {

    // Exercise 1.1
    // def next[B](g: A => WithCounter[B]): WithCounter[B] = i => {
    def next[B](g: A => WithState[B, Int]): WithState[B, Int] = i => {
      val (r, i1) = f(i)
      g(r)(i1)
    }
  }
}
