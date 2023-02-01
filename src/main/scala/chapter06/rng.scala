package chapter06

trait RNG:
  def nextInt: (Int, RNG)
  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    rng.nextInt match {
      case (i, rng) if i == Int.MinValue => (Int.MaxValue, rng)
      case (i, rng) if i < 0 => (-i, rng)
      case default => default
    }
  
  def double(rng: RNG): (Double, RNG) = 
    rng.nonNegativeInt(rng) match {
      case (0, rng) => (0.0, rng)
      case (1, rng) => (1 / Int.MaxValue, rng)
      case (i, rng) => (1 / i, rng)
    }

  

case class SimpleRNG(seed: Long) extends RNG:
  def nextInt: (Int, RNG) =
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)