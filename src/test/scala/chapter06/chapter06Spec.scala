package chapter06

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class chapter06Spec
    extends AnyFreeSpec
    with Matchers
    with TableDrivenPropertyChecks {
  "6.1" - {
    "nonNegativeInt" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val (n, nextRng) = rng.nonNegativeInt(rng)
        n should be >= 0
        nextRng
      }
    }
  }

  "6.2" - {
    "double" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val (n, nextRng) = rng.double(rng)
        n should (be >= 0.0 and be < 1.0)
        nextRng
      }
    }
  }

}
