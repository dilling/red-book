package chapter06

type Rand[+A] = RNG => (A, RNG)

trait RNG:
  def nextInt: (Int, RNG)
  val int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(f andThen unit)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
    
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = 
    rng =>
      val (a, nextRng) = r(rng)
      f(a)(nextRng)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, rng) if i == Int.MinValue => (Int.MaxValue, rng)
      case (i, rng) if i < 0             => (-i, rng)
      case default                       => default
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val double =
    map(nonNegativeInt){ 
      case 0 => 0.0
      case n => 1.0 / n
    }

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (int, rng1) = rng.nextInt
    val (double, rng2) = rng.double(rng1)
    ((int, double), rng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val (double, rng1) = rng.double(rng)
    val (int, rng2) = rng1.nextInt
    ((double, int), rng2)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (double1, rng1) = rng.double(rng)
    val (double2, rng2) = rng.double(rng1)
    val (double3, rng3) = rng.double(rng2)
    ((double1, double2, double3), rng3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    (1 to count).foldRight((List.empty[Int], rng)){
      case (_, (ls, rng)) => {
        val (int, nextRng) = nextInt
        (int :: ls, nextRng)
      }
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = 
    rng =>
      rs.foldRight((List.empty[A], rng)){
        case (rand, (ls, nextRng)) =>
          val (a, newRng) = rand(nextRng)
          (a :: ls, newRng)
      }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if i + (n-1) - mod >= 0 then unit(mod) 
      else nonNegativeLessThan(n)
    }
    
    
      

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
