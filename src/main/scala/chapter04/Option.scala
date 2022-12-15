package chapter04

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(a) => Some(f(a))
    case None => None

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(a => f(a)).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match
    case Some(get) => get
    case None => default
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = 
    map(a => Some(a)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = 
    flatMap(a => if f(a) then Some(a) else None)

object Option:
  def apply[A](a: A): Option[A] = Some(a)

  def none[A]: Option[A] = None

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(x => b.map(y => f(x,y)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = 
    as.foldRight(Option(List.empty[A]))((acc, opt) => map2(acc, opt)(_ :: _))
