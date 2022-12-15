package chapter04

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): Either[E, B] = 
    flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match 
    case Left(e) => Left(e)
    case Right(a) => f(a)

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case _: Left[E, A] => b
    case r: Right[E, A] => r

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => that.map(b => f(a,b)))
