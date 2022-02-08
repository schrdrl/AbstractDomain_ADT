package Experimenting

import AList._
import org.scalatest.funsuite.AnyFunSuite

class UnionWidenTest extends AnyFunSuite {
  /*********************
   *Tests union_AList  *
   *********************/


  test("Union ANil and ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil

    assert(b.union_AList(c,c) == b.ANil)
  }

  test("Union ANil and AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    val d = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val e = b.AMany(d)

    assert(b.union_AList(c,e) == e)
    assert(b.union_AList(e,c) == b.union_AList(c,e))
  }

  test("Union ANil and ASome"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    val d = b.intervals.Interval(IntegerVal(-1), IntegerVal(4))
    val e = b.intervals.Interval(IntegerVal(6), IntegerVal(8))
    val f = b.ACons(d, c)
    val g = b.ACons(d, b.AMany(e))

    val h = b.intervals.Interval(IntegerVal(-1), IntegerVal(8))

    assert(b.union_AList(c,f) == b.AMany(d))
    assert(b.union_AList(c,f) == b.union_AList(f,c))
    assert(b.union_AList(c,g) == b.AMany(h))
    assert(b.union_AList(c,g) == b.union_AList(g,c))
  }

  test("Union AMany and AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(4))
    val d = b.intervals.Interval(IntegerVal(6), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(-1), IntegerVal(8))
    val f = b.AMany(c)
    val g = b.AMany(d)

    assert(b.union_AList(f,g) == b.AMany(e))
    assert(b.union_AList(f,g) == b.union_AList(g,f))
  }

  test("Union ACons and AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(4))
    val d = b.intervals.Interval(IntegerVal(6), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(-1), IntegerVal(8))
    val f = b.AMany(c)
    val g = b.ACons(d, b.ANil)

    assert(b.union_AList(f,g) == b.AMany(e))
    assert(b.union_AList(f,g) == b.union_AList(g,f))

    val h = b.intervals.Interval(IntegerVal(-13), IntegerVal(-5))
    val i = b.ACons(d, b.AMany(h))
    val j = b.intervals.Interval(IntegerVal(-13), IntegerVal(8))

    assert(b.union_AList(f,i) == b.AMany(j))
    assert(b.union_AList(f,i) == b.union_AList(i,f))
  }

  test("Union ACons and ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(4))
    val d = b.intervals.Interval(IntegerVal(6), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(-1), IntegerVal(8))
    val f = b.intervals.Interval(IntegerVal(-13), IntegerVal(-5))
    val g = b.intervals.Interval(IntegerVal(-13), IntegerVal(8))

    val h =  b.ACons(c, b.ANil)
    val i = b.ACons(d, b.ANil)
    val j = b.ACons(c, b.ACons(d, b.ANil))
    val k = b.ACons(d, b.ACons(d, b.AMany(f)))

    assert(b.union_AList(h,i) == b.ACons(e,b.ANil))
    assert(b.union_AList(h,i) == b.union_AList(i,h))
    assert(b.union_AList(h,j) == b.ACons(c,b.AMany(d)))
    assert(b.union_AList(h,j) == b.union_AList(j,h))
    assert(b.union_AList(h,k) == b.ACons(e,b.AMany(g)))
    assert(b.union_AList(h,k) == b.union_AList(k,h))
    assert(b.union_AList(j,k) == b.ACons(e,b.ACons(d, b.AMany(f))))
    assert(b.union_AList(j,k) == b.union_AList(k,j))
  }


  /*********************
   *Tests widen_AList  *
   *********************/
  test("While-Loop with AList"){
    /* int i = 0
     * list xs = Nil
     * while(*){
     *    xs = Cons(i, xs)
     *    i++
     * }
     */
    val a = Intervals.Unbounded
    val b = ALists(a)
    var i = 0 //initial
    val axs1 = b.ANil //initial
    val i_head1 = b.intervals.Interval(IntegerVal(i), IntegerVal(i))
    val axs2 = b.ACons(i_head1, axs1)
    val axs3 = b.widen_AList(axs1, axs2)
    var axs4 = axs3

    val x = b.intervals.Interval(IntegerVal(0), IntegerInf)

    while (i<4){
      var i_head2 = b.intervals.Lattice.widen(i_head1, b.intervals.Interval(IntegerVal(i), IntegerVal(i)))
      var axs5 = b.ACons(i_head2, axs4)
      var axs6 = b.widen_AList(axs4,axs5)

      print("(" +i +")" +"interval head: ")
      println(b.intervals.Interval(IntegerVal(i), IntegerVal(i)))
      print("(" +i +")" +"axs before loop: ")
      if(i == 0) println(axs1)
      else println(axs4)

      if(i>1) assert(axs4 == b.AMany(x))

      if(i != 0) assert(i_head2 == x)

      print("(" +i +")" +"after loop: ")
      if (i==0) println(axs2)
      else println(axs5)

      if(i>1) assert(axs5 == b.ACons(x, b.AMany(x)))

      print("(" +i +")" +"after widen: ")
      if(i==0) println(axs3)
      else println(axs6)
      if(i != 0) assert(axs6 == b.AMany(x))

      print("axs for next iteration: ")
      axs4 = axs6
      println(axs4)
      println("")

      if(i != 0) assert(axs4 == b.AMany(x))
      i += 1


    }
  }


  test("widen with ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil

    val d = b.intervals.Interval(IntegerVal(-1), IntegerVal(4))
    val e = b.intervals.Interval(IntegerVal(6), IntegerVal(8))
    val f = b.intervals.Interval(IntegerVal(-1), IntegerVal(8))

    val g = b.AMany(d)
    val h = b.ACons(e, c) //ACons([6,8], ANil)
    val i = b.ACons(e, g) //ACons([6,8], AMany([-1,4]))
    val j = b.ACons(d, b.ACons(e,c)) //ACons([-1,4], ACons([6,8], ANil))

    assert(b.widen_AList(c,g) == b.AMany(d))
    assert(b.widen_AList(h,c) == b.AMany(e))
    assert(!(b.widen_AList(h,c) == b.ANil))

    assert(b.widen_AList(c,i) == b.AMany(b.intervals.Interval(IntegerNegInf, IntegerVal(8)))) //AMany([-∞;8])
    assert(b.widen_AList(i,c) == b.widen_AList(c,i))

    assert(b.widen_AList(c,j) == b.AMany(b.intervals.Interval(IntegerVal(-1), IntegerInf)))
    assert(b.widen_AList(j,c) == b.widen_AList(c,j))
  }

  test("widen with ACons"){
    //ACons([1,1], ACons([2,2], ANil)) ∇ ACons([1,1], ACons([1,1], ACons([5,5],ANil))) =
    // ACons([1,1], ACons([1,2], AMany([5,5])))

    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(1), IntegerVal(1))
    val d = b.intervals.Interval(IntegerVal(2), IntegerVal(2))
    val e = b.intervals.Interval(IntegerVal(5), IntegerVal(5))
    val f = b.intervals.Interval(IntegerVal(1), IntegerVal(2))

    val g = b.ACons(c, b.ACons(d, b.ANil))
    val h = b.ACons(c, b.ACons(c, b.ACons(e, b.ANil)))
    val i = b.intervals.Interval(IntegerNegInf, IntegerVal(2))
    val j = b.intervals.Interval(IntegerVal(1), IntegerInf)

    assert(b.widen_AList(g,h) == b.ACons(c, b.ACons(i, b.AMany(e)))) //returns: ACons([1;1],ACons([-∞;2],AMany([5;5])))
    assert(b.widen_AList(h,g) == b.ACons(c, b.ACons(j, b.AMany(e)))) //returns: ACons([1;1],ACons([1;∞],AMany([5;5])))

    val k = b.AMany(f)
    assert(b.widen_AList(g,k) == b.AMany(j)) //returns: AMany([1;∞])
    assert(b.widen_AList(k,g) == b.AMany(f)) //returns: AMany([1;2])
    assert(b.widen_AList(h,k) == b.AMany(j)) //returns: AMany([1;∞])
    assert(b.widen_AList(k,h) == b.AMany(j)) //returns: AMany([1;∞])
  }

  test("widen with AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.AMany(c)
    val f = b.AMany(d)

    assert(b.widen_AList(e,f) == b.AMany(b.intervals.Interval(IntegerVal(-1), IntegerInf))) //returns: AMany([-1;∞])
    assert(b.widen_AList(e,e) == e) //returns: AMany([-1;3])
    assert(b.widen_AList(f,e) == b.AMany(b.intervals.Interval(IntegerNegInf, IntegerVal(8)))) //returns: AMany([-∞;8])
  }



  /***********************
   *Tests widen_AOption  *
   ***********************/

  test("widen with ANone"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ANone
    val f = b.ASome(c)
    val g = b.AMaybe(d)

    assert(b.widen_AOptionAInt(e,e) == e) //returns: ANone
    assert(b.widen_AOptionAInt(e,f) == b.AMaybe(c)) //returns: AMaybe([-1;3])
    assert(b.widen_AOptionAInt(e,g) == g) //returns: AMaybe([5;8])
    assert(b.widen_AOptionAInt(g,e) == g) //returns: AMaybe([5;8])
  }

  test("widen with ASome"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(8), IntegerVal(12))

    val f = b.ASome(c)
    val g = b.AMaybe(d)
    val h = b.ASome(e)

    assert(b.widen_AOptionAInt(f,f) == f) //returns: ASome([-1;3])
    assert(b.widen_AOptionAInt(g,f) == b.AMaybe(b.intervals.Interval(IntegerNegInf, IntegerVal(8)))) //returns: AMaybe([-∞;8])
    assert(b.widen_AOptionAInt(f,g) == b.AMaybe(b.intervals.Interval(IntegerVal(-1), IntegerInf))) //returns: AMaybe([-1;∞])
    assert(b.widen_AOptionAInt(f,g) != b.widen_AOptionAInt(g,f))
    assert(b.widen_AOptionAInt(f,h) == b.ASome(b.intervals.Interval(IntegerVal(-1), IntegerInf))) //returns: ASome([-1;∞])
  }

  test("widen with AMaybe"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(8), IntegerVal(12))

    val f = b.AMaybe(c)
    val g = b.AMaybe(d)
    val h = b.ASome(e)

    val i = b.intervals.Interval(IntegerVal(-1), IntegerInf)
    val j = b.intervals.Interval(IntegerNegInf, IntegerVal(8))
    val k = b.intervals.Interval(IntegerNegInf, IntegerVal(12))

    assert(b.widen_AOptionAInt(f,f) == f) //AMaybe([-1;3])
    assert(b.widen_AOptionAInt(g,f) == b.AMaybe(j)) //AMaybe([-∞;8])
    assert(b.widen_AOptionAInt(f,g) == b.AMaybe(i)) //AMaybe([-1;∞])
    assert(b.widen_AOptionAInt(f,h) == b.AMaybe(i)) //AMaybe([-1;∞])
    assert(b.widen_AOptionAInt(h,f) == b.AMaybe(k)) //AMaybe([-∞;12])
  }

  test("widen AOption[AList]"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ANone
    val f = b.ASome(b.ANil)
    val g = b.AMaybe(b.ANil)
    val h = b.ASome(b.AMany(c))
    val i = b.AMaybe(b.AMany(c))
    val j = b.ASome(b.AMany(d))
    val k = b.AMaybe(b.AMany(d))

    val l = b.ASome(b.ACons(c, b.ANil))
    val m = b.AMaybe(b.ACons(c, b.AMany(c)))
    val n = b.AMaybe(b.ACons(c, b.AMany(d)))
    val o = b.ASome(b.ACons(c, b.ACons(c, b.ANil)))
    val p = b.AMaybe(b.ACons(c, b.ACons(d, b.ANil)))

    println(b.widen_AOptionAList(e,f)) //ANone, ASome(ANil)
    println(b.widen_AOptionAList(e,g)) //ANone, AMaybe(ANil)
    println(b.widen_AOptionAList(f,h)) //ASome(ANil), ASome(AMany)
    println(b.widen_AOptionAList(h,i)) //ASome(AMany), AMaybe(AMany)
    println(b.widen_AOptionAList(h,k)) //ASome(AMany), AMaybe(AMany)
    println("")

    println(b.widen_AOptionAList(l,m)) //ASome(ACons), AMaybe(ACons)
    println(b.widen_AOptionAList(m,n)) //AMaybe(ACons), AMaybe(ACons)
    println(b.widen_AOptionAList(m,o)) //AMaybe(ACons), ASome(ACons)
    println(b.widen_AOptionAList(o,p)) //ASome(ACons), AMaybe(ACons)
    println(b.widen_AOptionAList(i,p)) //AMaybe(AMany), AMaybe(ACons)
  }


  test("flatten_AList"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))
    val e = b.ANil
    val f = b.AMany(c)
    val g = b.ACons(c, b.ANil)
    val h = b.ACons(c, b.ACons(d, b.AMany(c)))

    val i = b.intervals.Interval(IntegerVal(0), IntegerVal(0))
    val j = b.ACons(i, b.ACons(c, b.ACons(d, b.ANil)))

    val k = b.intervals.Interval(IntegerVal(9), IntegerVal(10))
    val l = b.ACons(i, b.ACons(d, b.ACons(k, b.ANil)))

    println(b.flatten_AList(e))
    println(b.flatten_AList(f))
    println(b.flatten_AList(g))
    println(b.flatten_AList(h))
    println(b.flatten_AList(j))
    println(b.flatten_AList(l))

  }

  test("reverse"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(9), IntegerVal(10))
    val f = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    val g = b.ANil
    val h = b.AMany(c)
    val i = b.ACons(c, b.ANil)
    val j = b.ACons(c, b.ACons(d, b.AMany(c)))
    val k = b.ACons(f, b.ACons(c, b.ACons(d, b.ANil)))
    val l = b.ACons(f, b.ACons(d, b.ACons(e, b.ANil)))
    val m = b.ACons(f, b.ACons(d, b.ACons(e, b.AMany(c))))

    println(b.reverse(g))
    println(b.reverse(h))
    println(b.reverse(i))
    println(b.reverse(j))
    println(b.reverse(k))
    println(b.reverse(l))
    println(b.reverse(m))

  }

  test("widen AState"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))
    val e = b.ANil
    val f = b.ACons(c, b.ANil)

    val state1 = b.AState(Map( ("AInt", c),("AList", e)))
    val state2 = b.AState(Map( ("AInt", d),("AList", f)))

    val widenAState = b.widen_AState(state1, state2, Set("AInt", "AList"))
    println(widenAState)


  }
}
