package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class ATestTests extends AnyFunSuite {

  test("General functionality: AIf"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(2),IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(1),IntegerVal(1))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))

    val f = b.ANil
    val g = b.AMany(c)
    val h = b.ACons(d, b.AMany(e))

    //AState
    val state1 = b.AState(Map( ("AList", f), ("AInt", c), ("ABool", b.ATrue)))
    val state2 = b.AState(Map( ("AList", g), ("AInt", d), ("ABool", b.ATrue)))
    val state3 = b.AState(Map( ("AList", h), ("AInt", e), ("ABool", b.ATrue)))

    val aStates = Set(state1, state2, state3)
    println("AStates: " +aStates + "\n")

    //ATest
    val test = b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))
    val tests = Set(test)
    val aTest = b.ATest(tests)
    println("ATest: "+aTest + "\n")

    //AStmt
    val op1 = b.AState(Map( ("ABinOp", "-"), ("operator", d), ("operand", "AInt")))
    val op2 = b.AState(Map(("AUnOp", "aTail"),("operand", "AList")))

    val expressions = Set(op1, op2)

    val stmt_left = b.AStmt(expressions)
    val stmt_right = b.AStmt(Set())



    val aif = b.AIf(aTest,stmt_left, stmt_right)
    println("AIf: "+aif + "\n")

    val aif_exe = aif.execute(aStates)
    println("AIf after execution: " +aif_exe +"\n")





  }

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
