package Experimenting
import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class AppendingPrependingTests extends AnyFunSuite {

  test("+:"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(4))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val f = b.intervals.Interval(IntegerVal(8),IntegerVal(10))

    val g = b.ANil
    val h = b.AMany(c)
    val i = b.ACons(c, b.ANil)
    val j = b.ACons(c, b.AMany(d))
    val k = b.ACons(c, b.ACons(d, b.AMany(d)))

    println(b.+:(e, g))
    println(b.+:(e, h))
    println(b.+:(f, i))
    println(b.+:(f, j))
    println(b.+:(f, k))


  }
  test(":+"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(4))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val f = b.intervals.Interval(IntegerVal(8),IntegerVal(10))

    val g = b.ANil
    val h = b.AMany(c)
    val i = b.ACons(c, b.ANil)
    val j = b.ACons(c, b.AMany(d))
    val k = b.ACons(c, b.ACons(d, b.AMany(d)))


    println(b.:+(g,e))
    println(b.:+(h,e))
    println(b.:+(h,f))
    println(b.:+(i,f))
    println(b.:+(j,f))
    println(b.:+(k,f))
  }

  test("++"){

    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(4))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val f = b.intervals.Interval(IntegerVal(8),IntegerVal(10))

    val g = b.ANil
    val h = b.AMany(c)
    val i = b.ACons(c, b.ANil)
    val j = b.ACons(c, b.AMany(d))
    val k = b.ACons(c, b.ACons(d, b.AMany(d)))

    println(b.++(g,h))
    println(b.++(h,g))
    println(b.++(h,i))
    println(b.++(i,h))
    println("")
    println(b.++(j,g))
    println(b.++(k,g))
    println(b.++(j,i))

  }

}
