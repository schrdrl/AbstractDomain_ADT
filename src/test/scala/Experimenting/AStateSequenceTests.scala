package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class AStateSequenceTests extends AnyFunSuite {


  test("AState") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AState(Map(("AInt", c), ("ABool", b.AFalse), ("AList", b.AMany(c)), ("AOption[AInt]", b.ANone)))
    val e = b.AState(Map(("ABool", b.AFalse)))

    println(e.lookup("ABool"))
    println(e.values.exists(("Int", 3) == _))
    println(e.values.exists(_._1 == "AInt"))
    println(e.values.exists(_._1 == "ABool"))

  }


  test("Assign any value") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AState(Map(("AInt", c), ("ABool", b.AFalse), ("AList", b.AMany(c)), ("AOption[AInt]", b.ANone)))
    val e = b.AState(Map(("ABool", b.AFalse)))

    val f = b.AAssign("ABool", b.AConst(b.ATrue)).execute(Set(d, e))
    println(f)
    println(f.head.lookup("ABool"))
    println(f.tail.head.lookup("ABool"))

    val g = b.AAssign("AList", b.AConst(b.ANil)).execute(Set(d, e))
    println(g)
    println(g.head.lookup("ABool"))
    println(g.tail.head.lookup("ABool"))

  }


  test("ifIsNil") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AMany(c)
    val e = b.ANil
    val f = b.ACons(c, b.ANil)
    val g = b.ACons(c, b.AMany(c))

    val (h1, h2) = b.ifIsNil(d)
    println(h1, h2)

    val (i1, i2) = b.ifIsNil(e)
    println(i1, i2)

    val (j1, j2) = b.ifIsNil(f)
    println(j1, j2)

    val (k1, k2) = b.ifIsNil(g)
    println(k1, k2)
  }


  test("Add and subtract AInt") {
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(8), IntegerVal(5))
    val e = b.AState(Map(("AInt", c), ("ABool", b.AFalse), ("AList", b.AMany(c)), ("AOption[AInt]", b.ANone)))
    val f = b.AState(Map(("ABool", b.AFalse)))

    val g1 = b.ABinOp(b.AConst(c), "+", b.AConst(d)).evaluate(e)
    val h1 = b.ABinOp(b.AConst(c), "-", b.AConst(d)).evaluate(e)
    val g2 = b.ABinOp(b.AConst(c), "+", b.AConst(d)).evaluate(f)
    val h2 = b.ABinOp(b.AConst(c), "-", b.AConst(d)).evaluate(f)

    println(g1)
    println(g2)
    println(h1)
    println(h2)

  }


}
