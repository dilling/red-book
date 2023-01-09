package chapter04

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import chapter04.Option._
import org.scalatest.prop.TableDrivenPropertyChecks
import chapter04.Either
import chapter04.Either._

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
      (List(1.0,1,4), Some(2)),
      (List(1.0,2,3), Some(2.0/3.0)),
      (List(), None),
    )

    forEvery(table) { (l, expected) =>
      variance(l) shouldBe expected
    }
  }

  "4.3" in {
    val table = Table(
      ("age", "tickets", "expected"),
      ("2", "3", Some(6)),
      ("2", "", None),
    )

    forEvery(table) { (age, tickets, expected) =>
      parseInsuranceRateQuote(age, tickets) shouldBe expected
    }
  }

  "4.4" in {
    val table = Table(
      ("list", "expected"),
      (List(Option(2), Option(3)), Some(List(2,3))),
      (List(Option(2), None), None),
    )

    forEvery(table) { (list, expected) =>
      sequence(list) shouldBe expected
    }
  }

  "4.5" in {
    val table = Table(
      ("list", "expected"),
      (List(2, 3, 4), None),
      (List(2, 4), Some(List(1,2))),
    )

    forEvery(table) { (list, expected) =>
      traverse(list)(a => if a % 2 == 0 then Some(a / 2) else None) shouldBe expected
    }
  }

  "4.6" - {
    "map" in {
      val table = Table(
        ("either", "expected"),
        (Right(1), Right(2)),
        (Left("a"), Left("a")),
      )
      
      forEvery(table) { (either, expected) =>
        either.map(_ * 2) shouldBe expected
      }
    }

    "flatmap" in {
      val table = Table(
        ("either", "expected"),
        (Right(1), Right(2)),
        (Left("a"), Left("a")),
        (Right(0), Left("err")),
      )
      
      forEvery(table) { (either, expected) =>
        either.flatMap(d => if (d == 0) then Left("err") else Right(2 / d)) shouldBe expected
      }
    }

    "orElse" in {
      val table = Table(
        ("either", "expected"),
        (Right(1), Right(1)),
        (Left(1), Right(3)),
      )

      forEvery(table) { (either, expected) =>
        either.orElse(Right(3)) shouldBe expected
      }
    }

    "map2" in {
      val table = Table(
        ("left", "right", "expected"),
        (Right(1), Left(2), Left(2)),
        (Left(2), Right(3), Left(2)),
        (Left(1), Left(2), Left(1)),
        (Right(2), Right(3), Right(6)),
      )

      forEvery(table) { (left, right, expected) =>
        left.map2(right)(_ * _) shouldBe expected
      }
    }
  }
}
