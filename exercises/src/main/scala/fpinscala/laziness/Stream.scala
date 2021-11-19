package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](
      z: => B
  )(
      f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f)
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) =>
      p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, t) =>
      n match {
        case 1 => cons(h(), empty)
        case _ => cons(h(), t().take(n - 1))
      }
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(_, t) =>
      n match {
        case 1 => t()
        case _ => t().drop(n - 1)
      }

  }

  // def takeWhile(p: A => Boolean): Stream[A] = this match {
  //   case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
  //   case _ => empty
  // }
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else empty
    )

  // def forAll(p: A => Boolean): Boolean = this match {
  //   case Cons(h, t) => if p(h()) then t().forAll(p) else false
  //   case Empty => true
  // }
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // def headOption: Option[A] = this match {
  //   case Empty => None
  //   case Cons(h, _) => Some(h())
  // }

  def headOption: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(pred: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) =>
    if (pred(a)) cons(a, b)
    else b
  )

  def append[B>:A](a: Stream[B]): Stream[B] = foldRight(a)((x, y) => cons(x, y))

  def flatMap[B](fn: A => Stream[B]) = foldRight(empty[B])((a, b) => fn(a).append(b))

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Empty, Empty) => true
    case (Cons(hl, tl), Cons(hr, tr)) => (hl() == hr()) && tl().startsWith(tr())
  }

  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def mapUnfold[B](fn: A => B): Stream[B] = unfold(this)(a => a match {
    case Empty => None
    case Cons(h, t) => Some((fn(h()), t()))
  })

  def takeUnfold(n: Int): Stream[A] = unfold((this, 1))({
    case (Cons(h, t), 1) => Some(h(), (empty, 0))
    case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
    case _ => None
  })

  def takeWhileUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithUnfold[B, C](other: Stream[B])(fn: (A, B) => C): Stream[C] = unfold((this, other)) {
    case (Cons(hl, tl), Cons(hr, tr)) => Some((fn(hl(), hr()), (tl(), tr())))
    case _ => None
  }

  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, other)) {
    case (Cons(hl, tl), Cons(hr, tr)) => Some((Some(hl()), Some(hr())), (tl(), tr()))
    case (Cons(hl, tl), Empty) => Some((Some(hl()), None), (tl(), Empty))
    case (Empty, Cons(hr, tr)) => Some((None, Some(hr())), (Empty, tr()))
    case _ => None
  }

  def tails(): Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s  => Some(s, s drop 1)
  } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(z, Stream(z))((a, cur) => {
      lazy val saved = cur
      val n = f(a, saved._1)
      (n, cons(n, saved._2))
    })._2

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None           => Empty
    case (Some((a, s))) => cons(a, unfold(s)(f))
  }

  def ones_1() = unfold(1)(_ => Some((1, 1)))

  def constant_1[A](x: A) = unfold(x)(_ => Some((x, x)))

  def from_1(n: Int) = unfold(n)(x => Some((x, x + 1)))



}
