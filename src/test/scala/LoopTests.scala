import Abstraction.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class LoopTests extends AnyFunSuite {
  /** *******************************************************
   * tests: Loop Abstract                                  *
   * Tests are just using the abstract type AList          *
   * ****************************************************** */
  test("Loop Abstract 1 (n = 0, ASome)") {
    /**
     * int n
     * list xs
     * while != isNil(xs){
     * xs = xs.tail
     * n++
     * }
     */
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))

    var xs: b.AList = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil)))
    var ys: b.AOption[b.AList] = b.ASome(xs)
    var n = 0

    println("befor loop: " + xs)
    while (b.isNil(xs) != b.ATrue) {
      var b_AUnknown = true
      while (b.isNil(xs) != b.ATrue && b_AUnknown == true) {
        println("(" + n + ")before xs: " + xs)
        ys = b.aTail(xs)
        println("(" + n + ")ys: " + ys)
        if (b.isNil(xs) == b.AUnknown) { //to ensure termination of the loop (case AUnknown)
          b_AUnknown = false
        }
        xs = b.justAList(ys) //TODO get value AList out of b.AOption[b.AList]
        println("(" + n + ")after xs: " + xs)
        println("")
        n += 1
      }
    }
    println("after loop: " + xs)
    assert(xs == b.ANil)
    assert(n >= 0)
  }

  test("Loop Abstract 1 (n = 0, AMaybe)") {
    /**
     * int n
     * list xs
     * while != isNil(xs){
     * xs = xs.tail
     * n++
     * }
     */
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    var b_AUnknown = true
    var xs: b.AList = b.AMany(c)
    var ys: b.AOption[b.AList] = b.AMaybe(xs)
    var n = 0

    println("befor loop: " + xs)

    while (b.isNil(xs) != b.ATrue && b_AUnknown == true) {
      println("(" + n + ")before xs: " + xs)
      ys = b.aTail(xs)
      println("(" + n + ")ys: " + ys)
      if (b.isNil(xs) == b.AUnknown) { //to ensure termination of the loop (case AUnknown)
        b_AUnknown = false
      }
      xs = b.justAList(ys) //TODO get value AList out of b.AOption[b.AList]
      println("(" + n + ")after xs: " + xs)
      println("")
      n += 1

    }
    println("after loop: " + xs)
    assert(n >= 0)

  }

  test("Append value(Abstract)") {
    /**
     * int n
     * xs <0>
     * while(*){
     * i++
     * xs = i::xs
     * }
     */
    var n = 0
    val a = Intervals.Unbounded
    val b = ALists(a)
    var axs: b.AList = b.ANil
    var i = b.intervals.Interval(IntegerVal(n), IntegerVal(n))

    while (n < 4) {
      val i_head = b.intervals.Interval(IntegerVal(n), IntegerVal(n))
      i = i_head
      println("(" + n + ")axs before: " + axs)
      n += 1
      axs = b.ACons(i_head, axs)
      println("(" + (n - 1) + ")axs: " + axs)
      println("")
    }
    assert(b.aHead(axs) == b.ASome(i))
    assert(b.aHead(axs) == b.ASome(i)) //TODO override ==
    println(b.aHead(axs))
    println(b.ASome(i))

  }

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
        if (b.isNil(axs) == b.AUnknown) {
          b_AUnknown = false
        }
        axs = b.justAList(ys) //TODO get value AList out of b.AOption[b.AList]

        println("(" + n + ")after xs: " + xs)
        println("(" + n + ")after axs: " + axs)
        println("")
        n += 1
      }
    }
    println("after loop: " + xs)
    println("after loop: " + axs)
    assert(xs == Nil)
    assert(axs == b.ANil)
    assert(n >= 0)
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
        if (b.isNil(axs) == b.AUnknown) {
          b_AUnknown = false
        }
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

    //TODO
  }

}
