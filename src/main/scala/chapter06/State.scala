package chapter06

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, next) = underlying(s)
        f(a)(next)

    def map[B](f: A => B): State[S, B] =
      underlying.flatMap(f andThen unit)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(
      f: (A, B) => C
  ): State[S, C] =
    for {
      a <- sa
      b <- sb
    } yield f(a, b)

  def sequence[S, A](ls: List[State[S, A]]): State[S, List[A]] = 
    initial => 
      ls.foldRight((List.empty[A], initial)){
        case (state, (xs, s)) =>
          val (a, next) = state(s)
          (a :: xs, next)
      }
