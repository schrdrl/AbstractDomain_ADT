package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class EqualityTests extends AnyFunSuite {

  test("=== on ABool") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c: b.ABool = b.ATrue
    val d = b.AFalse

    println(b.!==(c, c))
    println(b.!==(d, d))
    println(b.===(c, c))
    println(b.===(c, d))
  }

  test("=== on AInt") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(3), IntegerVal(8))

    println(b.===(c, c))
    println(b.===(d, c))
    println(b.===(d, e))
  }


  test("=== on AList") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3), IntegerVal(8))

    val e = b.ANil
    val f = b.AMany(c)
    val g = b.AMany(d)
    val h = b.ACons(c, b.ANil)
    val i = b.ACons(c, b.AMany(c))
    val j = b.ACons(d, b.AMany(c))

    println(b.===(e, b.ANil)) //ATrue
    println(b.===(e, f)) //ATrue
    println(b.===(f, g)) //AFalse
    println("")
    println(b.===(f, h)) //AFalse
    println(b.===(e, h)) //AFalse
    println(b.===(f, i)) //ATrue
    println("")
    println(b.===(f, j)) //AFalse
    println(b.===(j, g)) //AFalse
    println("")

    val m = b.ACons(c,b. ACons(c, b.ACons(c,b.ANil)))
    val n = b.ACons(c,b. ACons(c, b.AMany(c)))
    println(b.===(m, n)) //AFalse

  }

  test("=== on AOption[AInt]") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3), IntegerVal(8))

    val e = b.ANone
    val f = b.ASome(c)
    val g = b.AMaybe(c)
    val h = b.AMaybe(d)

    println(b.===(e,b.ANone : b.AOption[b.AInt])) //Otherwise: double definition (ANone, ANone) for AOption[AInt] and AOption[AList]
    println(b.===(e,f))
    println(b.===(g,f))
    println(b.===(e,g))
    println(b.===(g,h))
  }

  test("=== on AOption[AList]") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3), IntegerVal(8))

    val e = b.ANil
    val f = b.AMany(c)
    val g = b.AMany(d)
    val h = b.ACons(c, b.ANil)
    val i = b.ACons(c, b.AMany(c))
    val j = b.ACons(d, b.AMany(c))

    val k = b.ANone
    val l = b.ASome(e)
    val m = b.AMaybe(e)

    println(b.===(k,l))
    println(b.===(k,m))
    println(b.===(m,l))

    val n = b.AMaybe(f)
    val o = b.AMaybe(h)
    val p = b.ASome(f)
    val q = b.ASome(h)
    println(b.===(n,o))
    println(b.===(q,p))

  }

  //TODO === AState

}
