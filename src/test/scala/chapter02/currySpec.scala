package chapter02

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class currySpec extends AnyFunSuite with Matchers:
  test("curry") {
    val f = curry((a: Int, b: Int) => a + b)
    f(3)(5) shouldBe 8
  }

  test("uncurry") {
    val f = uncurry((a: Int) => (b: Int) => a + b)
    f(2, 3) shouldBe 5
  }
