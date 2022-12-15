package chapter04

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then Option.none[Double]
  else Option(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))