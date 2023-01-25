package chapter05

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList._

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, _) => Some(h())

  def toList: List[A] = this match
    case Empty      => List.empty
    case Cons(h, t) => h() :: t().toList

  def take(n: Int): LazyList[A] = this match
    case Empty       => Empty
    case _ if n <= 0 => LazyList.empty
    case Cons(h, t)  => LazyList.cons(h(), t().take(n - 1))

  def drop(n: Int): LazyList[A] = this match
    case Empty       => Empty
    case l if n <= 0 => l
    case Cons(_, t)  => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Empty                 => Empty
    case Cons(h, _) if !p(h()) => Empty
    case Cons(h, t)            => LazyList.cons(h(), t().takeWhile(p))

  def foldRight[B](acc: => B)(f: (A, => B) => B): B =
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _          => acc

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def headOption2: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))
