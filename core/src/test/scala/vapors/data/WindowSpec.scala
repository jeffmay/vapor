package com.rallyhealth

package vapors.data

import cats.Order
import org.scalacheck.Arbitrary
import org.scalacheck.ops._
import org.scalactic.Equivalence
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import scala.collection.immutable.NumericRange
import scala.reflect.runtime.universe.{typeOf, TypeTag}

class WindowSpec extends AnyWordSpec {

  class BoundedWindowTests[A : Arbitrary : Order](buildWindow: (A, A) => Window[A]) {
    import cats.syntax.order._

    private def assertWithBoundedWindow(
      testLowerBound: (Window[A], A) => Unit = (_, _) => (),
      testUpperBound: (Window[A], A) => Unit = (_, _) => (),
    ): Unit = {
      forAll { (a1: A, a2: A) =>
        whenever(a1 != a2) {
          val lowerBound = a1 min a2
          val upperBound = a1 max a2
          val window = buildWindow(lowerBound, upperBound)
          testLowerBound(window, lowerBound)
          testUpperBound(window, upperBound)
        }
      }
    }

    def itIsABoundedWindow(
      includeMin: Boolean,
      includeMax: Boolean,
    ): Unit = {
      "return true for contains when the value is inside the bounds" in {
        forAll { (a1: A, a2: A, v: A) =>
          whenever(a1 != a2 && v != a1 && v != a2) {
            val lowerBound = a1 min a2
            val upperBound = a1 max a2
            val window = buildWindow(lowerBound, upperBound)
            if (v < upperBound && v > lowerBound) {
              assert(window.contains(v))
            } else {
              assert(!window.contains(v))
            }
          }
        }
      }

      if (includeMin) {
        itIsAStartInclusiveWindow()
      } else {
        itIsAStartExclusiveWindow()
      }
      if (includeMax) {
        itIsAnEndInclusiveWindow()
      } else {
        itIsAnEndExclusiveWindow()
      }
    }

    def itIsAStartExclusiveWindow(): Unit = {
      "be exclusive in the lower bound" in {
        assertWithBoundedWindow(
          testLowerBound = (window, lowerBound) => assert(!window.contains(lowerBound)),
        )
      }
    }

    def itIsAStartInclusiveWindow(): Unit = {
      "be inclusive in the lower bound" in {
        assertWithBoundedWindow(
          testLowerBound = (window, lowerBound) => assert(window.contains(lowerBound)),
        )
      }
    }

    def itIsAnEndExclusiveWindow(): Unit = {
      "be exclusive in the upper bound" in {
        assertWithBoundedWindow(
          testUpperBound = (window, upperBound) => assert(!window.contains(upperBound)),
        )
      }
    }

    def itIsAnEndInclusiveWindow(): Unit = {
      "be inclusive in the upper bound" in {
        assertWithBoundedWindow(
          testUpperBound = (window, upperBound) => assert(window.contains(upperBound)),
        )
      }
    }
  }

  "Window" when {

    "fromRange() is given a standard Int Range" when {

      "exclusive (X until Y)" should {
        val tests = new BoundedWindowTests[Int]((min, max) => Window.fromRange(min until max))
        behave like tests.itIsABoundedWindow(
          includeMin = true,
          includeMax = false,
        )
      }

      "inclusive (X to Y)" should {
        val tests = new BoundedWindowTests[Int]((min, max) => Window.fromRange(min to max))
        tests.itIsABoundedWindow(
          includeMin = true,
          includeMax = true,
        )
      }
    }

    def aNumericRangeIgnoringStepSize[A : Integral : Arbitrary : Equivalence](): Unit = {
      val I = Integral[A]
      implicit val A: Order[A] = Order.fromOrdering
      val step2 = I.plus(I.one, I.one)

      "exclusive" should {
        val exclusiveRange = new BoundedWindowTests[A](
          (min, max) => Window.fromRange(NumericRange(min, max, step2)),
        )
        exclusiveRange.itIsABoundedWindow(includeMin = true, includeMax = false)
      }

      "inclusive" should {
        val inclusiveRange = new BoundedWindowTests[A](
          (min, max) => Window.fromRange(NumericRange.inclusive(min, max, step2)),
        )
        inclusiveRange.itIsABoundedWindow(includeMin = true, includeMax = true)
      }
    }

    "fromRange() is given an Int NumericRange" when {
      behave like aNumericRangeIgnoringStepSize[Int]()
    }

    "fromRange() is given a Long NumericRange" when {
      behave like aNumericRangeIgnoringStepSize[Long]()
    }

    def allWindowTests[A : Arbitrary : Ordering : TypeTag](): Unit = {
      implicit val order: Order[A] = Order.fromOrdering
      val typeName = typeOf[A].toString

      s"given values of type $typeName" when {

        "between()" should {
          val tests = new BoundedWindowTests[A](
            (min, max) => Window.between(min, max),
          )
          behave like tests.itIsABoundedWindow(
            includeMin = true,
            includeMax = false,
          )
        }

        "betweenInclusive()" should {
          val tests = new BoundedWindowTests[A](
            (min, max) => Window.betweenInclusive(min, max),
          )
          behave like tests.itIsABoundedWindow(
            includeMin = true,
            includeMax = true,
          )
        }

        "between() is called given includeMin and includeMax set to false" should {
          val tests = new BoundedWindowTests[A](
            (min, max) => Window.between(min, includeMin = false, max, includeMax = false),
          )
          behave like tests.itIsABoundedWindow(
            includeMin = false,
            includeMax = false,
          )
        }

        "between() is called given includeMin set to false and includeMax set to true" should {
          val tests = new BoundedWindowTests[A](
            (min, max) => Window.between(min, includeMin = false, max, includeMax = true),
          )
          behave like tests.itIsABoundedWindow(
            includeMin = false,
            includeMax = true,
          )
        }

        def assertContainsValue(
          createWindow: A => Window[A],
          chooseBoundary: (A, A) => A,
          inclusive: Boolean,
        ): Unit = {
          forAll { (a1: A, a2: A) =>
            whenever(inclusive || a1 != a2) {
              val boundary = chooseBoundary(a1, a2)
              val window = createWindow(boundary)
              val value = if (a1 === boundary) a2 else a1
              assert(window.contains(value))
              if (inclusive) {
                assert(window.contains(boundary))
              }
            }
          }
        }

        import cats.syntax.order._

        "greaterThan() contains a value greaterThan a given window boundary" in {
          assertContainsValue(Window.greaterThan(_), _ min _, inclusive = false)
        }

        "greaterThanOrEqual() contains a value greaterThanOrEqualTo a given window boundary" in {
          assertContainsValue(Window.greaterThanOrEqual(_), _ min _, inclusive = true)
        }

        "lessThan() contains a value lessThan a given window boundary" in {
          assertContainsValue(Window.lessThan(_), _ max _, inclusive = false)
        }

        "lessThanOrEqual() contains a value lessThanOrEqualTo a given window boundary" in {
          assertContainsValue(Window.lessThanOrEqual(_), _ max _, inclusive = true)
        }
      }
    }

    locally {
      allWindowTests[Int]()
      allWindowTests[Long]()
      allWindowTests[LocalDate]()
      allWindowTests[LocalTime]()
      allWindowTests[LocalDateTime]()
      allWindowTests[ZonedDateTime]()
    }
  }
}
