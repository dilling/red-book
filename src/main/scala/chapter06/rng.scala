package chapter06

trait RNG:
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, rng) if i == Int.MinValue => (Int.MaxValue, rng)
      case (i, rng) if i < 0             => (-i, rng)
      case default                       => default
    }

  def double(rng: RNG): (Double, RNG) =
    rng.nonNegativeInt(rng) match
      case (0, rng) => (0.0, rng)
      case (1, rng) => (1 / Int.MaxValue, rng)
      case (i, rng) => (1 / i, rng)

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
    
      

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
