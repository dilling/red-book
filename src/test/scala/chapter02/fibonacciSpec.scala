package chapter02

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class fibonacciSpec extends AnyWordSpec with Matchers:
  "fib" should {
    "calculate fibonacci" in {
      // fib(0) shouldBe 0
      fib(5) shouldBe 5
      fib(7) shouldBe 13
    }
  }
