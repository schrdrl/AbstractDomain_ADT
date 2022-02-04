package Experimenting

import AList.IntegerW.integerWToInt
import AList.{ALists, IntegerInf, IntegerNegInf, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class IntervalsTests extends AnyFunSuite {
  test("Print correct representation of 'Positiv' Interval"){
    val a = Intervals.Positive
    //println(a)
    assert(a.toString == "Intervals(0,∞)")
  }

  test("Print correct representation of Interval from 0 to 10"){
    val a = Intervals(IntegerVal(0), IntegerVal(10))
    //println(a)
    assert(a.toString == "Intervals(0,10)")
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

    assert(a.union_Interval(b,c) == e)
    assert(a.union_Interval(b,d) == f)
    assert(a.union_Interval(c,d) == g)
    assert(a.union_Interval(b,b) == b)
  }

  test("Widen Intervals"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(0), IntegerVal(0))
    val c = a.Interval(IntegerVal(1), IntegerVal(1))
    val d = a.Interval(IntegerVal(0), IntegerVal(1))
    val e = a.Interval(IntegerVal(2), IntegerVal(2))
    val f = a.Interval(IntegerVal(0), IntegerInf)
    val g = a.Interval(IntegerNegInf, IntegerVal(1))
    val h = a.Interval(IntegerVal(1), IntegerInf)
    val i = a.Interval(IntegerNegInf, IntegerInf)

    assert(a.Lattice.widen(b,b) == b)
    assert(a.Lattice.widen(b,c) == f)
    assert(a.Lattice.widen(c,b) == g)
    assert(a.Lattice.widen(b,d) == f)
    assert(a.Lattice.widen(c,e) == h)
    assert(a.Lattice.widen(f,h) == f)
    assert(a.Lattice.widen(h,f) == i)
  }

  test("Greatest lower bound (Meet/Infimum)"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(0), IntegerVal(10))
    val c = a.Interval(IntegerVal(1), IntegerVal(1))
    val d = a.Interval(IntegerVal(0), IntegerVal(1))
    val e = a.Interval(IntegerVal(-1), IntegerVal(13))
    val f = a.Interval(IntegerVal(12), IntegerVal(18))
    val g = a.Interval(IntegerVal(-10), IntegerVal(25))
    val h = a.Interval(IntegerVal(15), IntegerVal(21))

    println("glb:") //Union of the max values of lb and ub
    println(a.Lattice.glb(b,b)) //[0;10]
    println(a.Lattice.glb(b,c)) //[1;10] union would be:[0;10]
    println(a.Lattice.glb(b,d)) //[0;10]
    println(a.Lattice.glb(b,e)) //[0;2]
    println(a.Lattice.glb(c,c)) //[1;1]
    println(a.Lattice.glb(c,d)) //[1;1]
    println(a.Lattice.glb(c,e)) //[1;13]
    println(a.Lattice.glb(d,e)) //[0;13]
    println(a.Lattice.glb(f,g)) //[12;25]
    println(a.Lattice.glb(g,f)) //[12;25]
    println(a.Lattice.glb(h,g)) //[15;25] union would be:[-10;25]
    //Interval is smaller than i1 and i2
    //"Kleinstmögliches gemeinsames Intervall" != intersection, could be []
  }

  test("Least Upper Bound (Join/Supremum)"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(0), IntegerVal(10))
    val c = a.Interval(IntegerVal(1), IntegerVal(1))
    val d = a.Interval(IntegerVal(0), IntegerVal(1))
    val e = a.Interval(IntegerVal(-1), IntegerVal(13))
    val f = a.Interval(IntegerVal(12), IntegerVal(18))
    val g = a.Interval(IntegerVal(-10), IntegerVal(25))
    val h = a.Interval(IntegerVal(15), IntegerVal(21))

    println("lub:")
    println(a.Lattice.lub(b,b)) //[0;10]
    println(a.Lattice.lub(b,c)) //[0;10]
    println(a.Lattice.lub(b,d)) //[0;10]
    println(a.Lattice.lub(b,e)) //[-1;13]
    println(a.Lattice.lub(c,c)) //[1;1]
    println(a.Lattice.lub(c,d)) //[0;1]
    println(a.Lattice.lub(c,e)) //[-1;13]
    println(a.Lattice.lub(d,e)) //[-1;13]
    println(a.Lattice.lub(f,e)) //[-1;18]
    println(a.Lattice.lub(f,g)) //[-10;25]
    println(a.Lattice.lub(g,f)) //[-10;25]
    println(a.Lattice.lub(h,g)) //[-10;25]
    //union, i get a larger interval out of i1 and i2
    //"größtmögliches gemeinsames Intervall"
  }

  test("Interval contains interval"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(2), IntegerVal(4))
    val c = a.Interval(IntegerVal(-1), IntegerVal(5))
    val d = a.Interval(IntegerVal(3), IntegerVal(8))
    val e = a.Interval(IntegerVal(5), IntegerVal(5))

    //check if first interval is in second
    assert(a.contains_Interval(b,c)) //true
    assert(!a.contains_Interval(c, b)) //false
    assert(!a.contains_Interval(b,d)) //false
    assert(!a.contains_Interval(d,b)) //false
    assert(!a.contains_Interval(d, e)) //false
    assert(a.contains_Interval(e, d)) //true
    assert(a.contains_Interval(d, d)) //true

  }

  test("Intersect intervals"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(2), IntegerVal(4))
    val c = a.Interval(IntegerVal(-1), IntegerVal(5))
    val d = a.Interval(IntegerVal(3), IntegerVal(8))
    val e = a.Interval(IntegerVal(5), IntegerVal(5))
    val f = a.Interval(IntegerVal(8), IntegerVal(25))
    val g = a.Interval(IntegerVal(7), IntegerVal(25))
    val h = a.Interval(IntegerInf, IntegerNegInf)
    val i = a.Interval(IntegerVal(8), IntegerVal(8))

    assert(a.intersect_Interval(b,c) == b) //[2;4]
    assert(a.intersect_Interval(c,b) == b) //[2;4]
    assert(a.intersect_Interval(c,b) == a.intersect_Interval(b,c))
    assert(a.intersect_Interval(c,c) == c) //[-1;5]
    assert(a.intersect_Interval(e,b) == h) //[∞;-∞]
    assert(a.intersect_Interval(b,e) == h) //[∞;-∞]
    assert(a.intersect_Interval(e,b) == a.intersect_Interval(b,e))
    assert(a.intersect_Interval(d,f) == i) //[8;8]
    assert(a.intersect_Interval(g,f) == f) //[8,25]
    assert(a.intersect_Interval(f,g) == f) //[8,25]
  }

  test("==="){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(2), IntegerVal(4))
    val c = a.Interval(IntegerVal(-1), IntegerVal(5))
    val d = a.Interval(IntegerVal(2), IntegerVal(4))
    val e = a.Interval(IntegerVal(5), IntegerVal(5))

    assert(!a.===(b,c))
    assert(a.===(b,b))
    assert(a.===(b,d))
    assert(!a.===(e,c))
  }

  test("<="){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(2), IntegerVal(4))
    val c = a.Interval(IntegerVal(-1), IntegerVal(5))
    val d = a.Interval(IntegerVal(2), IntegerVal(2))
    val e = a.Interval(IntegerVal(5), IntegerVal(5))

    println(a.Lattice.<=(b,b))
    println(a.Lattice.<=(b,c))
    println(a.Lattice.<=(c,b))
  }

  test("IntegerWToInt"){
    val a = Intervals.Unbounded
    val b = a.Interval(IntegerVal(2), IntegerVal(4))
    val c = a.Interval(IntegerNegInf, IntegerVal(5))
    val d = a.Interval(IntegerVal(-1), IntegerInf)

    println(integerWToInt(d.lb))
    println(integerWToInt(d.ub))
    println(integerWToInt(c.lb))



  }

  test("IntegerW =="){
    val a = Intervals.Unbounded
    val b = IntegerVal(0)
    val c = IntegerVal(1)
    val d = IntegerVal(0)

    println(b == c)
    println(b == d)

  }


/*
//TODO AIntEqual needs improvement
  test("AIntEqual"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(3),IntegerVal(8))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(1),IntegerVal(2))
    val f = b.intervals.Interval(IntegerVal(3),IntegerVal(5))
    val g = b.intervals.Interval(IntegerVal(1),IntegerVal(8))

    val h = b.AIntEqual
    println(h.positive(c,d))
    println(h.negative(c,d))
    println("")

    println(h.positive(c,e))
    println(h.negative(c,e))
    println("")

    println(h.positive(c,f))
    println(h.negative(c,f))
    println("")

    println(h.positive(c,g))
    println(h.negative(c,g))

  }


 */
}
