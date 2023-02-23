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

  "6.3" - {
    "intDouble" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val ((int, double), nextRng) = rng.intDouble(rng)
        nextRng
      }
    }

    "doubleInt" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val ((double, int), nextRng) = rng.doubleInt(rng)
        nextRng
      }
    }

    "double3" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val ((one, two, three), nextRng) = rng.double3(rng)
        nextRng
      }
    }
  }

  "6.4" - {
    "ints" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val (ints, nextRng) = rng.ints(3)(rng)
        ints.size shouldBe 3
        nextRng
      }
    }
  }

  "6.6" - {
    "randIntDouble" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val ((int, double), nextRng) = rng.randDoubleInt(rng)
        nextRng
      }
    }

    "randDoubleInt" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val ((double, int), nextRng) = rng.randDoubleInt(rng)
        nextRng
      }
    }
  }

  "6.7" - {
    "sequence" in {
      val rng: RNG = SimpleRNG(42)
      val len = 10
      val ls = List.fill(len)(rng.int)

      val (res, _) = rng.sequence(ls)(rng)
      res.length shouldBe len
      res match
        case one :: two :: _ => one should not be (two)
        case _               => fail("invalid list")

    }
  }

  "6.8" - {
    "nonNegativeLessThan" in {
      (1 to 10).foldLeft(SimpleRNG(42): RNG) { case (rng, _) =>
        val (n, nextRng) = rng.nonNegativeLessThan(5)(rng)
        n should (be >= 0 and be < 5)
        nextRng
      }
    }
  }

  "6.11" - {
    "simulateMachine" - {
      "unlock if there is candy left" in {
        val machine = Machine(true, 1, 4)
        val inputs = List(Input.Coin)

        val ((candies, coins), updatedMachine) =
          simulateMachine(inputs).run(machine)

        coins shouldBe 5
        candies shouldBe 1
        updatedMachine shouldBe Machine(false, 1, 5)
      }

      "dispense candy when turned if unlocked" in {
        val machine = Machine(false, 1, 4)
        val inputs = List(Input.Turn)

        val ((candies, coins), updatedMachine) =
          simulateMachine(inputs).run(machine)

        coins shouldBe 4
        candies shouldBe 0
        updatedMachine shouldBe Machine(true, 0, 4)
      }

      "do nothing when turned and locked" in {
        val machine = Machine(true, 1, 4)
        val inputs = List(Input.Turn)

        val ((candies, coins), updatedMachine) =
          simulateMachine(inputs).run(machine)

        coins shouldBe 4
        candies shouldBe 1
        updatedMachine shouldBe machine
      }

      "do nothing when coin inserted and unlocked" in {
        val machine = Machine(false, 1, 4)
        val inputs = List(Input.Coin)

        val ((candies, coins), updatedMachine) =
          simulateMachine(inputs).run(machine)

        coins shouldBe 4
        candies shouldBe 1
        updatedMachine shouldBe machine
      }

      "do nothing when out of candy" in {
        val machine = Machine(false, 0, 4)
        val inputs = List(Input.Coin, Input.Turn)

        val ((candies, coins), updatedMachine) =
          simulateMachine(inputs).run(machine)

        coins shouldBe 4
        candies shouldBe 0
        updatedMachine shouldBe machine
      }

      "return number of coins and candies in machine" in {
        val machine = Machine(true, 3, 4)
        val inputs = List.fill(5)(List(Input.Turn, Input.Coin)).flatten

        val ((candies, coins), updatedMachine) =
          simulateMachine(inputs).run(machine)

        coins shouldBe 7
        candies shouldBe 0
        updatedMachine shouldBe Machine(true, 0, 7)

      }
    }
  }

}
