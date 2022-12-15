package chapter04

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import chapter04.Option._
import org.scalatest.prop.TableDrivenPropertyChecks
import cha

class chapter04Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
  "4.1" - {
    "map" in {
      Option(4).map(_ * 2) shouldBe Some(8)
      Option.none[Int].map(_ * 2) shouldBe None
    }

    "flatmap" in {
      def f(a: Int) = if (a == 0) then None else Some(a / 2)
      Option(4).flatMap(f) shouldBe Some(2)
      Option.none[Int].flatMap(f) shouldBe None
    }

    "getOrElse" in {
      Option(1).getOrElse(2) shouldBe 1
      Option.none[Int].getOrElse(2) shouldBe 2
    }

    "orElse" in {
      Option(1).orElse(Some(2)) shouldBe Some(1)
      Option.none[Int].orElse(Some(2)) shouldBe Some(2)
      Option.none[Int].orElse(None) shouldBe None
    }

    "filter" in {
      def f(a: Int) = a % 2 == 0
      Option(2).filter(f) shouldBe Some(2)
      Option(1).filter(f) shouldBe None
      
    }
  }

  "4.2" in {
    val table = Table(
      ("seq", "variance"),
      (List(1,2,3), 1),
      (List(1,3,5), 4),
    )

    forEvery(table) { (l, expected) =>
      variance(l) shouldBe expected
    }
  }
}
