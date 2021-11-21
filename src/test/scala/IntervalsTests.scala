import Abstraction.{IntegerInf, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class IntervalsTests extends AnyFunSuite {
  test("Print correct representation of 'Positiv' Interval"){
    val a = Intervals.Positive
    //println(a)
    assert((a.toString).equals("Intervals(0,∞)") )
  }

  test("Print correct representation of Interval from 0 to 10"){
    val a = Intervals(IntegerVal(0), IntegerVal(10))
    //println(a)
    assert((a.toString).equals("Intervals(0,10)") )
  }

  test("Interval contains integer-value"){

    val i = 1
    val a = Intervals.Positive
    val b = a.Interval(IntegerVal(0), IntegerVal(10))
    assert(a.contains(b,i))
  }

  test("0 is concrete Element of positive interval"){
    val i = 0
    val a = Intervals.Positive
    val b = a.Interval(IntegerVal(0), IntegerInf)
    assert(a.contains(b,i))
  }

  test("List of integers are concrete elements of positive interval"){
    //TODO
  }

}
