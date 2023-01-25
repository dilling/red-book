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

  "5.7" - {
    "map" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(1, 2), List(2, 4)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.map(_ * 2).toList shouldBe expected
      }
    }

    "filter" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(1, 2, 3, 4), List(2, 4)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.filter(_ % 2 == 0).toList shouldBe expected
      }
    }

    "append" in {
      val table = Table(
        ("a", "b", "expected"),
        (LazyList(1, 2), LazyList(3, 4), List(1, 2, 3, 4)),
        (LazyList(1, 2), LazyList.empty, List(1, 2)),
        (LazyList.empty, LazyList(3, 4), List(3, 4))
      )

      forEvery(table) { (listA, listB, expected) =>
        listA.append(listB).toList shouldBe expected
      }
    }

    "flatMap" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(1, 2), List(1, 1, 2, 2)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.flatMap(x => LazyList(x, x)).toList shouldBe expected
      }
    }
  }

  "5.8" in {
    LazyList.continually(1).take(3).toList shouldBe List(1, 1, 1)
  }

  "5.9" in {
    LazyList.from(2).takeWhile(_ <= 4).toList shouldBe List(2, 3, 4)
  }

  "5.10" in {
    LazyList.fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  "5.11" in {
    LazyList
      .unfold(0)(s =>
        if (s < 5) Some((s * 2, s + 1))
        else None
      )
      .toList shouldBe List(0, 2, 4, 6, 8)
  }

  "5.12" - {
    "fibs" in {
      LazyList.fibs2.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
    }

    "from" in {
      LazyList.from2(2).takeWhile(_ <= 4).toList shouldBe List(2, 3, 4)
    }

    "continually" in {
      LazyList.continually2(1).take(3).toList shouldBe List(1, 1, 1)
    }

    "ones" in {
      LazyList.ones2.take(3).toList shouldBe List(1, 1, 1)
    }
  }

  "5.13" - {
    "map" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(1, 2), List(2, 4)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.map2(_ * 2).toList shouldBe expected
      }
    }

    "take" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(1, 2), List(1, 2)),
        (LazyList(1, 2, 3, 4), List(1, 2, 3)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.take2(3).toList shouldBe expected
      }
    }

    "takeWhile" in {
      val table = Table(
        ("list", "expected"),
        (LazyList(2), List(2)),
        (LazyList(1, 2, 3, 4), List(1, 2)),
        (LazyList.empty, List.empty)
      )

      forEvery(table) { (list, expected) =>
        list.takeWhile3(_ <= 2).toList shouldBe expected
      }
    }

    "zipWith" in {
      val table = Table(
        ("listA", "listB", "expected"),
        (LazyList(1,2,3), LazyList(2,3), List(2,6)),
        (LazyList(1,2,3), LazyList(2,3,4,5), List(2,6,12)),
        (LazyList.empty, LazyList(2,3), List.empty),
      )

      forEvery(table) { (listA, listB, expected) =>
        listA.zipWith(listB, _ * _).toList shouldBe expected
      }
    }

    "zipAll" in {
      val table = Table(
        ("listA", "listB", "expected"),
        (LazyList(1,2,3), LazyList(2,3), List((Some(1),Some(2)), (Some(2),Some(3)), (Some(3), None))),
        (LazyList(1,2,3), LazyList(2,3,4,5), List((Some(1),Some(2)), (Some(2),Some(3)), (Some(3),Some(4)), (None, Some(5)))),
        (LazyList.empty, LazyList(2), List((None, Some(2)))),
      )

      forEvery(table) { (listA, listB, expected) =>
        listA.zipAll(listB).toList shouldBe expected
      }
    }
  }
}
