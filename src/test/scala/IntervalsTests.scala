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
    val a = List(1,2,3,4)
    val b = List(-1,2,5)
    val c = Intervals.Positive
    val d = c.Interval(IntegerVal(0), IntegerInf)

    var bool1 = true
    var bool2 = true

    for(i <- a) {
       if(c.contains(d,i) == false){
         bool1 = false
       }
    }

    for(i <- b) {
      if(c.contains(d,i) == false){
        bool2 = false
      }
    }

    assert(bool1 == true)
    assert(bool2 == false)

  }

  test("Union intervals"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(5), IntegerVal(8))
    val c = a.Interval(IntegerVal(-1), IntegerVal(3))
    val d = a.Interval(IntegerVal(2), IntegerVal(4))

    val e = a.Interval(IntegerVal(-1), IntegerVal(8))
    val f = a.Interval(IntegerVal(2), IntegerVal(8))
    val g = a.Interval(IntegerVal(-1), IntegerVal(4))

    assert(a.union_Interval(b,c).equals(e))
    assert(a.union_Interval(b,d).equals(f))
    assert(a.union_Interval(c,d).equals(g))
    assert(a.union_Interval(b,b).equals(b))
  }

  test("Widen Intervals"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(0), IntegerVal(0))
    val c = a.Interval(IntegerVal(1), IntegerVal(1))
    val d = a.Interval(IntegerVal(0), IntegerVal(1))

    println(a.Lattice.widen(b,c))
    println(a.Lattice.widen(c,b))
    println(a.Lattice.widen(b,d))
  }

}
