package EvaluatingTests

import org.scalatest.funsuite.AnyFunSuite

import scala.util.control.Breaks.break

class IsPositive extends AnyFunSuite {
  test("Elements in List are positive (concrete") {
    var xs: List[Int] = List(9, 7, 4, 5) //only positive numbers
    var n = xs.head

    while (!xs.isEmpty) {
      if (n >= 0 && !xs.tail.isEmpty) {
        xs = xs.tail
        n = xs.head
      }
      else if (xs.tail.isEmpty) xs = xs.tail
      else {
        break;
      }
    }

    assert(xs.isEmpty)
    assert(n >= 0)
  }



}
