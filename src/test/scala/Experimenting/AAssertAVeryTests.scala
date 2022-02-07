package Experimenting
import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class AAssertAVeryTests extends AnyFunSuite {

  test("General functionality: AAssert"){
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

    val aStates = Set(state1, state2, state3)
    println("AStates: " +aStates + "\n")

    //ATest
    val test = b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))
    val tests = Set(test)
    val aTest = b.ATest(tests)
    println("ATest: "+aTest + "\n")

    //AAssert
    val as = b.AAssert(aTest)
    println(as)

    val as_exe = as.execute(aStates)
    println(as_exe +"\n")
    println("Is not Nil: " +as_exe._1)
    println("Is Nil: " +as_exe._2)

  }


  test("General functionality: AVerify"){
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

    val aStates = Set(state1, state2, state3)
    println("AStates: " +aStates + "\n")

    //ATest
    val test = b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))
    val tests = Set(test)
    val aTest = b.ATest(tests)
    println("ATest: "+aTest + "\n")

    //AVerify
    val av = b.AVerify(aTest)
    println(av)

    val av_exe = av.execute(aStates)

  }



}
