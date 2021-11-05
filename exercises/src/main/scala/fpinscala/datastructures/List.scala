package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) return l
    l match {
      case Nil => Nil
      case Cons(h, tail) => drop(tail, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def prod3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, (Nil: List[A]))((acc: List[A], curr: A) => Cons(curr, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B) = foldLeft(reverse(l), z)((b, a) => f(a, b))

  def append2[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)((curr: A, acc:List[A]) =>
    Cons(curr, acc)
  )

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append2)

  // I'd just write these with map
  def addOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((cur, acc) => Cons(cur + 1, acc))

  def doubleToList(l: List[Double]): List[String] =
    foldRight(l, List[String]())((a: Double, b: List[String]) => Cons(a.toString, b))

  // ok  so 3.19 was map

  // 3.20
  def flatMap[A](l: List[A])(f: A => List[A]): List[A] =
    flatten(map(l)(f))

  // 3.21
  def filter[A](l: List[A])(pred: A => Boolean): List[A] =
    flatMap(l)(a => if (pred(a)) List(a) else Nil)

  // 3.22
  def zipAdd(l: List[Int], r: List[Int]): List[Int] = {
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(lh + rh, zipAdd(lt, rt))
    }
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(lh, lt), Cons(rh, rt)) => Cons(f(lh, rh), zipWith(lt, rt)(f))
    }
}
