package chapter03

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import chapter03.List._

class chapter03Spec extends AnyFreeSpec with Matchers {

  "3.1" in {
    val result = List(1, 2, 3, 4, 5) match
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101

    result shouldBe 3
  }

  "3.2" in {
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  "3.3" in {
    setHead(4, List(1, 2, 3)) shouldBe List(4, 2, 3)
  }

  "3.4" in {
    drop(List(1,2,3), 2) shouldBe List(3)
  }

  "3.5" in {
    dropWhile(List(1,2,3), _ < 3) shouldBe List(3)
  }

  "3.6" in {
    init(List(1,2,3,4)) shouldBe List(1,2,3)
  }

  "3.8" in {
    // The data constructorcs can be implemented in terms of fold right
    info(s"${foldRight(List(1, 2, 3), Nil: List[Int], Cons(_, _))}")
  }

  "3.9" in {
    List.length(List(1,1,1)) shouldBe 3
  }

  "3.11" in {
    sumLeft(List(1,2,3,4)) shouldBe 10
    productLeft(List(1,2,3,4)) shouldBe 24
  }

  "3.12" in {
    reverse(List(1,2,3)) shouldBe List(3,2,1)
  }

  "3.13" in {
    foldRight2(List(1,2,3), 0, _ + _) shouldBe 6
  }

  "3.14" in {
    append(List(1,2), List(3,4)) shouldBe List(1,2,3,4)
  }

  "3.15" in {
    flatten(List(List(1,2), List(3,4))) shouldBe List(1,2,3,4)
  }

  "3.16" in {
    addOne(List(1,2,3)) shouldBe List(2,3,4)
  }

  "3.17" in {
    doubleToString(List(1,2,3)) shouldBe List("1.0", "2.0", "3.0")
  }

  "3.18" in {
    map(List(1,2,3), _ + 1) shouldBe List(2,3,4)
  }

  "3.19" in {
    filter(List(1,2,3,4), _ % 2 == 0) shouldBe List(2,4)
  }

  "3.20" in {
    flatMap(List(1,2,3), a => List(a,a)) shouldBe List(1,1,2,2,3,3)
  }
}
