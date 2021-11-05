package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 3.25
  def countNodes[A](t: Tree[A]): Int = {
    def go[A](n: Tree[A]): Int = {
      n match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + go(l) + go(r)
      }
    }
    go(t)
  }


  // 3.26
  def maxNode(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maxNode(l) max maxNode(r)
  }

  // 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // 3.28
  def mapTree[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(mapTree(l, f), mapTree(r, f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldMax(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def foldDepth[A](t: Tree[A]): Int = fold(t)(_ => 0)((lDepth, rDepth) => 1 + (lDepth max rDepth))

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => (Leaf(f(a))) : Tree[B])((l, r) => Branch(l, r))
}
