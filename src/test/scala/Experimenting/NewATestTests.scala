package Experimenting
import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class NewATestTests extends AnyFunSuite {

  test("General functionality: ATest"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(2),IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(1),IntegerVal(1))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))

    val f = b.ANil
    val g = b.AMany(c)
    val h = b.ACons(d, b.AMany(e))

    val state1 = b.AState(Map( ("AList", f), ("AInt", c), ("ABool", b.ATrue)))
    val state2 = b.AState(Map( ("AList", g), ("AInt", d), ("ABool", b.ATrue)))
    val state3 = b.AState(Map( ("AList", h), ("AInt", e), ("ABool", b.ATrue)))

    val test = b.AState(Map(("test", "ifIsNil"),("testedValue", "AList")))

    val tests = Set(test)

    val aTest = b.ATest(tests)
    println("ATest: "+aTest + "\n")

    val aStates = Set(state1, state2, state3)
    println("AStates: " +aStates + "\n")

    val neg = aTest.negative(aStates)
    println("negative Test: " +neg +"\n")

    val pos = aTest.positive(aStates)
    println("positive Test: " +pos +"\n")
  }


  test("ATestOp"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-3),IntegerVal(8))
    val d = b.intervals.Interval(IntegerVal(-3),IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))

    val f = b.ANil
    val g = b.AMany(c)
    val h = b.ACons(d, b.AMany(e))

    val state1 = b.AState(Map( ("AList", f), ("AInt", c), ("ABool", b.AFalse)))
    val state2 = b.AState(Map( ("AList", g), ("AInt", d), ("ABool", b.ATrue)))
    val state3 = b.AState(Map( ("AList", h), ("AInt", e), ("ABool", b.ATrue)))

    val aStates = Set(state1, state2, state3)
    println("AStates: " +aStates + "\n")

    val test1 = b.AState(Map(("test", "ifIsNil"),("testedValue", "AList")))
    val test2 = b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))
    val test3 = b.AState(Map(("test", "ifIsPositive"),("testedValue", "AInt")))
    val test4 = b.AState(Map(("test", "ifIsNegative"),("testedValue", "AInt")))
    val test5 = b.AState(Map(("test", "ifIsZero"),("testedValue", "AInt")))
    val test6 = b.AState(Map(("test", "ifIsATrue"),("testedValue", "ABool")))
    val test7 = b.AState(Map(("test", "ifIsAFalse"),("testedValue", "ABool")))

    val tests = Set(test1, test2, test3, test4, test5,test6,test7)

    val aTest = b.ATest(tests)
    println("ATest: "+aTest + "\n")


    val neg = aTest.negative(aStates)
    println("negative Test: " +neg +"\n")

    val pos = aTest.positive(aStates)
    println("positive Test: " +pos +"\n")



  }

}
