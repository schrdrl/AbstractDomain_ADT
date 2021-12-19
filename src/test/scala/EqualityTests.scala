import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class EqualityTests extends AnyFunSuite {

  test("=== on ABool") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c: b.ABool = b.ATrue
    val d = b.AFalse
    val e = b.AUnknown

    println(b.!==(c, c))
    println(b.===(c, c))
    println(b.===(c, d))
    println(b.===(c, e))
    println(b.!==(c, e))
    println(b.===(d, e))
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

    println(b.equals_AList(e, b.ANil))
    println(b.equals_AList(e, f))
    println(b.equals_AList(f, g))
    println(b.equals_AList(f, h))
    println(b.equals_AList(e, h))
    println(b.equals_AList(f, i))
    println(b.equals_AList(f, j))
    println(b.equals_AList(j, g))
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

    println(b.equals_AOption_AInt(e,b.ANone))
    println(b.equals_AOption_AInt(e,f))
    println(b.equals_AOption_AInt(g,f))
    println(b.equals_AOption_AInt(e,g))
    println(b.equals_AOption_AInt(g,h))
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

    println(b.equals_AOption_AList(k,l))
    println(b.equals_AOption_AList(k,m))
    println(b.equals_AOption_AList(m,l))

    val n = b.AMaybe(f)
    val o = b.AMaybe(h)
    val p = b.ASome(f)
    val q = b.ASome(h)
    println(b.equals_AOption_AList(n,o))
    println(b.equals_AOption_AList(q,p))

    //todo more tests

  }

}
