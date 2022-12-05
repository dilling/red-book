package chapter03

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] = 
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int = ints match
    case List.Nil => 0
    case List.Cons(head, tail) => head + sum(tail)

  def product(doubles: List[Double]): Double = doubles match
    case List.Nil => 1.0
    case Cons(0.0, _) => 0.0
    case List.Cons(head, tail) => head * product(tail)

  def tail(list: List[_]) = list match
    case Nil => Nil
    case Cons(head, tail) => tail

  def setHead[A](a: A, list: List[A]) = list match
    case Nil => List(a)
    case Cons(_, tail) => Cons(a, tail)

  def drop[A](as: List[A], n: Int): List[A] = 
    if n <= 0 then as
    else as match
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n-1)

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => as

  def init[A](as: List[A]): List[A] = as match
    case Nil => Nil
    case Cons(head, Cons(_, Nil)) => Cons(head, Nil)
    case Cons(head, tail) => Cons(head, init(tail))

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def length[A](as: List[A]): Int = 
    def go(as: List[A], l: Int): Int = as match
      case Nil => l
      case Cons(_, tail) => go(tail, l + 1)

    go(as, 0)

  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
    case Nil => acc
    case Cons(head, tail) => foldLeft(tail, f(acc, head), f)


  def sumLeft(as: List[Int]): Int = 
    foldLeft(as, 0, _ + _)

  def productLeft(as: List[Int]): Int = 
    foldLeft(as, 1, _ * _)

  def reverse[A](as: List[A]): List[A] = 
    foldLeft(as, Nil: List[A], (b, a) => Cons(a, b))

  def foldRight2[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, (b, a) => f(a,b))

  def append[A](a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1, a2, Cons(_, _))

  def flatten[A](as: List[List[A]]): List[A] = 
    foldRight(as, Nil: List[A], append)

  def addOne(as: List[Int]): List[Int] = 
    foldRight(as, Nil: List[Int], (a, b) => Cons(a + 1, b))

  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String], (a, b) => Cons(a.toString(), b))

  def map[A, B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil: List[B], (a, b) => Cons(f(a), b))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A], (a, b) => if f(a) then Cons(a, b) else b)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = 
    foldRight(as, Nil: List[B], (a, b) => append(f(a), b))
