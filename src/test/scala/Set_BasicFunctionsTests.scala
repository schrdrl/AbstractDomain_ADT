import AList.{IntegerVal, Intervals}
import AList_Set._
import org.scalatest.funsuite.AnyFunSuite


class Set_BasicFunctionsTests extends AnyFunSuite {

  /** *************
   * Tests aHead  *
   * ************ */

  test("aHead: single AList") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, b.ANil)
    println(b.aHead(Set(d)))
    println(b.aHead(Set(e)))
    println(b.aHead(Set(f)))
  }

  test("aHead: multiple ALists") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, b.ANil)
    println(b.aHead(Set(d, e, f)))
  }

  /** *************
   * Tests aTail  *
   * ************ */
  test("aTail: single AList") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, b.ANil)
    println(b.aTail(Set(d)))
    println(b.aTail(Set(e)))
    println(b.aTail(Set(f)))
  }

  test("aTail: multiple ALists") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, b.ANil)
    println(b.aTail(Set(d, e, f)))
  }


  /** *************
   * Tests isNil  *
   * ************ */

  test("isNil") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, b.ANil)

    println(b.isNil(Set(d)))
    println(b.isNil(Set(e)))
    println(b.isNil(Set(f)))
    println(b.isNil(Set(d, e, f)))
  }


  /** *************
   * Tests ifIsNil  *
   * ************ */

}
