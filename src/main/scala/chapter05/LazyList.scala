package chapter05

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

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
    foldRight(LazyList.empty[A])((a, b) => if (p(a)) LazyList.cons(a, b) else b)

  def headOption2: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => LazyList.cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) => if (f(a)) LazyList.cons(a, b) else b)

  def append[B >: A](ls: LazyList[B]): LazyList[B] =
    foldRight(ls)((a, b) => LazyList.cons(a, b))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) => f(a).append(b))

  def map2[B](f: A => B): LazyList[B] =
    LazyList.unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def take2(n: Int): LazyList[A] =
    LazyList.unfold((n, this)) {
      case (n, Cons(h, t)) if n > 0 => Some((h(), (n - 1, t())))
      case _                        => None
    }

  def takeWhile3(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this) {
      case Cons(h, t) if (p(h())) => Some(h(), t())
      case _                      => None
    }

  def zipWith[B, C](ls: LazyList[B], f: (A, B) => C): LazyList[C] =
    LazyList.unfold((this, ls)) {
      case (Cons(a, as), Cons(b, bs)) => Some(f(a(), b()), (as(), bs()))
      case _                          => None
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, that)) {
      case (Cons(a, as), Cons(b, bs)) =>
        Some((Some(a()), Some(b())), (as(), bs()))
      case (Cons(a, as), _) => Some((Some(a()), None), (as(), Empty))
      case (_, Cons(b, bs)) => Some((None, Some(b())), (Empty, bs()))
      case _                => None
    }

  def startsWith[A](prefix: LazyList[A]): Boolean = 
    zipAll(prefix).forAll {
      case (Some(a), Some(b)) => a == b
      case (_, None) => true
      case _ => false
    }

  def tails: LazyList[LazyList[A]] =
    LazyList.unfold(this)(l => l match
      case Cons(h, t) => Some((l, t()))
      case Empty => None 
    )

  def scanRight[B](z: B)(f: (A, => B) => B): LazyList[B] = 
    foldRight(LazyList(z))((a, b) => b match
      case Empty => Empty
      case Cons(h, t) => LazyList.cons(f(a, h()), b)
    )

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  def continually[A](a: A): LazyList[A] =
    cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  def fibs: LazyList[Int] =
    def go(x: Int, y: Int): LazyList[Int] =
      cons(x, go(y, x + y))

    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state).fold(LazyList.empty[A]) { case (a, s) =>
      cons(a, unfold(s)(f))
    }

  def fibs2: LazyList[Int] =
    unfold((0, 1)) { case (x, y) => Some((x, (y, x + y))) }

  def from2(n: Int): LazyList[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def continually2[A](a: A): LazyList[A] =
    unfold(a)(s => Some((s, s)))

  val ones2: LazyList[Int] = continually2(1)
