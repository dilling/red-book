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
  
