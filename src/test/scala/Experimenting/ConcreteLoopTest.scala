package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite


class ConcreteLoopTest extends AnyFunSuite {
  /**
   * Tests still need improvement but might be interesting/useful
   * when a translator between concrete and abstract lists is implemented
   */

  //TODO refactor Tests if necessary
  /** *******************************************************************************
   * Tests: Loop Concrete                                                          *
   * Experiments to connect the concrete list and the abstract list in Testcases   *
   * ******************************************************************************* */
  test("Loop Concrete 1") {
    /**
     * int n
     * xs = <1,2,3>
     * while(xs != Nil){
     * xs = xs.tail
     * n++
     * }
     * with isConcreteElementOf_List(xs, axs)
     */
    var n = 0
    var xs: List[Int] = List(1, 2, 3)
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val axs: b.AList = b.ACons(c, b.AMany(c))

    println("before loop: " + xs)
    while (xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue) {

      println("(" + n + ")before xs: " + xs)
      xs = xs.tail
      println("(" + n + ")after xs: " + xs)
      println("")
      n += 1
    }
    println("after loop: " + xs)
    assert(n >= 0)
    assert(xs == Nil)
    assert(axs == b.ACons(c, b.AMany(c)))
  }


  test("Loop Concrete 1 (ASome)") {
    /**
     * int n
     * List xs
     * AList axs
     * while(xs != Nil){
     * xs = xs.tail
     * axs = axs.tail
     * n++
     * }
     */
    var n = 0
    var xs: List[Int] = List(1, 2, 3)
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    var axs: b.AList = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil)))
    var ys: b.AOption[b.AList] = b.ASome(axs)

    while (xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue) {
      var b_AUnknown = true
      while (xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue && b_AUnknown == true) {
        println("(" + n + ")before xs: " + xs)
        println("(" + n + ")before axs: " + axs)
        xs = xs.tail

        ys = b.aTail(axs)
        //if (b.isNil(axs) == b.AUnknown) {
        b_AUnknown = false
     // }
      axs = b.justAList(ys)

      println("(" + n + ")after xs: " + xs)
      println("(" + n + ")after axs: " + axs)
      println("")
      n += 1
    }
  }
  // println("after loop: " + xs)
  //println("after loop: " + axs)
  //assert(xs == Nil)
  //assert(axs == b.ANil)
  //assert(n >= 0)
}

  test("Loop Concrete 1 (AMaybe)") {
    /**
     * int n
     * List xs
     * AList axs
     * while(xs != Nil){
     * xs = xs.tail
     * axs = axs.tail
     * n++
     * }
     */
    var n = 0
    var xs: List[Int] = List(1, 2, 3)
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    var axs: b.AList = b.AMany(c)
    var ys: b.AOption[b.AList] = b.AMaybe(axs)
    while (xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue) {
      var b_AUnknown = true
      while (xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue && b_AUnknown == true) {
        println("(" + n + ")before xs: " + xs)
        println("(" + n + ")before axs: " + axs)
        xs = xs.tail

        ys = b.aTail(axs)
       // if (b.isNil(axs) == b.AUnknown) {
          b_AUnknown = false
       // }
        axs = b.justAList(ys)

        println("(" + n + ")after xs: " + xs)
        println("(" + n + ")after axs: " + axs)
        println("")
        n += 1
      }
    }
    println("after loop: " + xs)
    println("after loop: " + axs)
    assert(n >= 0)
    assert(xs == Nil)
    assert(axs == b.AMany(c))
  }

  test("Append value(Concrete)") {
    /**
     * int n
     * xs <0>
     * while(*){
     * i++
     * xs = i::xs
     * }
     */
  }

}
