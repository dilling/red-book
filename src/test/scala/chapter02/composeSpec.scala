package chapter02

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class composeSpec extends AnyFunSuite with Matchers:
  test("compose") {
    val f = compose((b: Int) => b * 2, (a: Int) => a + 3)
    f(2) shouldBe 10
  }
