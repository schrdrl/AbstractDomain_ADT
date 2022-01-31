package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class ATestTests extends AnyFunSuite {
/*
  test("xsIsNilTest"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val e = b.AState(d, b.ANil)
    val f =  b.AState(d,b.ACons(c, b.ANil))
    val g =  b.AState(d,b.AMany(c))

    val h = b.xsIsNilTest
    println(h.positive(Set(e)))
    println(h.negative(Set(e)))
    println(h.positive(Set(f)))
    println(h.negative(Set(f)))
    println(h.positive(Set(g)))
    println(h.negative(Set(g)))
  }

  test("AIf"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val e = b.AState(d, b.ANil)
    val f =  b.AState(d,b.ACons(c, b.ANil))
    val g =  b.AState(d,b.AMany(c))

    val test = b.xsIsNilTest //ATest
    val stmt1 = b.Assign_SameValues
    val stmt2 = b.Subtract1

    val ifTest = b.AIf(test, stmt1, stmt2)
    println(ifTest.execute(Set(e)))
    println(ifTest.execute(Set(f)))
    println(ifTest.execute(Set(g)))
  }

  test("AWhile"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val e = b.AState(d, b.ANil)
    val f =  b.AState(d,b.ACons(c, b.ANil))
    val g =  b.AState(d,b.AMany(c))

    val test1 = b.xsIsNilTest //ATest
    val test2 = b.xsIsNotNilTest //ATest
    val stmt = b.Subtract1  //AStmt

    val whileTest1 = b.AWhile(test1, stmt)
    println(whileTest1.execute(Set(e)))
    println(whileTest1.execute(Set(f)))
    println(whileTest1.execute(Set(g)))

    val whileTest2 = b.AWhile(test2, stmt)
    println(whileTest2.execute(Set(e)))
    println(whileTest2.execute(Set(f)))
    println(whileTest2.execute(Set(g)))
  }


  test("nIsPositive"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(5))
    val e = b.AState(d, b.ANil)
    val f =  b.AState(c,b.ACons(c, b.ANil))
    val g =  b.AState(d,b.AMany(c))

    val h = b.nIsPositive
    println(h.positive(Set(e)))
    println(h.negative(Set(e)))
    println("")
    println(h.positive(Set(f)))
    println(h.negative(Set(f)))
    println("")
    println(h.positive(Set(g)))
    println(h.negative(Set(g)))
    println("")
    println(h.positive(Set(e,f,g)))
    println(h.negative(Set(e,f,g)))
  }

  test("nIsNegative"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(-5),IntegerVal(-1))
    val e = b.AState(d, b.ANil)
    val f =  b.AState(c,b.ACons(c, b.ANil))
    val g =  b.AState(d,b.AMany(c))

    val h = b.nIsNegative
    println(h.positive(Set(e)))
    println(h.negative(Set(e)))
    println("")
    println(h.positive(Set(f)))
    println(h.negative(Set(f)))
    println("")
    println(h.positive(Set(g)))
    println(h.negative(Set(g)))
    println("")
    println(h.positive(Set(e,f,g)))
    println(h.negative(Set(e,f,g)))
  }


  test("nEqualsZero"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val e = b.AState(d, b.ANil)
    val f =  b.AState(c,b.ACons(c, b.ANil))
    val g =  b.AState(d,b.AMany(c))

    val h = b.nEqualsZero
    println(h.positive(Set(e)))
    println(h.negative(Set(e)))
    println("")
    println(h.positive(Set(f)))
    println(h.negative(Set(f)))
    println("")
    println(h.positive(Set(g)))
    println(h.negative(Set(g)))
    println("")
    println(h.positive(Set(e,f,g)))
    println(h.negative(Set(e,f,g)))
  }

  test("AAssert"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(0),IntegerVal(0))
    val d = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))

    val e = b.nEqualsZero
    val f = b.AAssert(e)
    val g = b.AState(c, b.ANil)
    val h = b.AState(d, b.ANil)
    val i = b.AState(c, b.AMany(d))
    val j = b.AState(d,  b.AMany(c))
    f.execute(Set(g,h,i,j))
  }


 */
}
