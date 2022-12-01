package chapter02

import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite

class isSortedSpec extends AnyFunSuite with Matchers {
  test("isSorted") {
    isSorted(Array(1, 2, 3), _ > _) shouldBe true
    isSorted(Array(1, 2, 1), _ > _) shouldBe false
    isSorted(Array(3, 2, 1), _ < _) shouldBe true
    isSorted(Array(1, 2, 3), _ < _) shouldBe false
  }
}
