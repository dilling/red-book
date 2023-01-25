package chapter05

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import chapter05.LazyList

class chapter05Spec
    extends AnyFreeSpec
    with Matchers
    with TableDrivenPropertyChecks {
  "5.1" in {
    val table = Table(
      ("list", "expected"),
      (LazyList(1, 2), List(1, 2)),
      (LazyList.empty, List.empty)
    )

    forEvery(table) { (list, expected) =>
      list.toList shouldBe expected
    }
  }

  "5.2" - {
    "take" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(1, 2), List(1, 2)),
        (LazyList(1, 2, 3, 4), List(1, 2, 3)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.take(3).toList shouldBe expected
      }
    }

    "drop" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(1, 2), List.empty),
        (LazyList(1, 2, 3, 4), List(4)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.drop(3).toList shouldBe expected
      }
    }
  }

  "5.3" in {
    val table = Table(
      ("list", "expected"),
      (LazyList(2), List(2)),
      (LazyList(1, 2, 3, 4), List(1, 2)),
      (LazyList.empty, List.empty)
    )

    forEvery(table) { (list, expected) =>
      list.takeWhile(_ <= 2).toList shouldBe expected
    }
  }

  "5.4" in {
    val table = Table(
      ("list", "expected"),
      (LazyList(1, 2, 3), false),
      (LazyList(2, 4), true),
      (LazyList.empty, true)
    )

    forEvery(table) { (list, expected) =>
      list.forAll(_ % 2 == 0) shouldBe expected
    }
  }

  "5.5" in {
    val table = Table(
      ("list", "expected"),
      (LazyList(2), List(2)),
      (LazyList(1, 2, 3, 4), List(1, 2)),
      (LazyList.empty, List.empty)
    )

    forEvery(table) { (list, expected) =>
      list.takeWhile2(_ <= 2).toList shouldBe expected
    }
  }

  "5.6" in {
    val table = Table(
      ("list", "expected"),
      (LazyList(1, 2), Some(1)),
      (LazyList.empty, None)
    )

    forEvery(table) { (list, expected) =>
      list.headOption2 shouldBe expected
    }
  }

}
