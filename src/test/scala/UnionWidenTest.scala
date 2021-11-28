import Abstraction._
import org.scalatest.funsuite.AnyFunSuite

class UnionWidenTest extends AnyFunSuite {
  /***************
   *Tests union_AList  *
   **************/


  test("Union ANil and ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil

    assert(b.union_AList(c,c).equals(b.ANil))
  }

  test("Union ANil and AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    val d = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val e = b.AMany(d)

    assert(b.union_AList(c,e).equals(e))
    assert(b.union_AList(e,c).equals(b.union_AList(c,e)))
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

    assert(b.union_AList(c,f).equals(b.AMany(d)))
    assert(b.union_AList(c,f).equals(b.union_AList(f,c)))
    assert(b.union_AList(c,g).equals(b.AMany(h)))
    assert(b.union_AList(c,g).equals(b.union_AList(g,c)))
  }

  test("Union AMany and AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(4))
    val d = b.intervals.Interval(IntegerVal(6), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(-1), IntegerVal(8))
    val f = b.AMany(c)
    val g = b.AMany(d)

    assert(b.union_AList(f,g).equals(b.AMany(e)))
    assert(b.union_AList(f,g).equals(b.union_AList(g,f)))
  }

  test("Union ACons and AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(4))
    val d = b.intervals.Interval(IntegerVal(6), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(-1), IntegerVal(8))
    val f = b.AMany(c)
    val g = b.ACons(d, b.ANil)

    assert(b.union_AList(f,g).equals(b.AMany(e)))
    assert(b.union_AList(f,g).equals(b.union_AList(g,f)))

    val h = b.intervals.Interval(IntegerVal(-13), IntegerVal(-5))
    val i = b.ACons(d, b.AMany(h))
    val j = b.intervals.Interval(IntegerVal(-13), IntegerVal(8))

    assert(b.union_AList(f,i).equals(b.AMany(j)))
    assert(b.union_AList(f,i).equals(b.union_AList(i,f)))
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

    assert(b.union_AList(h,i).equals(b.ACons(e,b.ANil)))
    assert(b.union_AList(h,i).equals(b.union_AList(i,h)))
    assert(b.union_AList(h,j).equals(b.ACons(c,b.AMany(d))))
    assert(b.union_AList(h,j).equals(b.union_AList(j,h)))
    assert(b.union_AList(h,k).equals(b.ACons(e,b.AMany(g))))
    assert(b.union_AList(h,k).equals(b.union_AList(k,h)))
    assert(b.union_AList(j,k).equals(b.ACons(e,b.ACons(d, b.AMany(f)))))
    assert(b.union_AList(j,k).equals(b.union_AList(k,j)))
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
    val axs2 = b.ACons(b.intervals.Interval(IntegerVal(i), IntegerVal(i)), axs1)
    val axs3 = b.widen_AList(axs1, axs2)
    var axs4 = axs3

    while (i<4){

      var axs5 = b.ACons(b.intervals.Interval(IntegerVal(i), IntegerVal(i)), axs4)
      var axs6 = b.widen_AList(axs4,axs5)

      print("(" +i +")" +"before loop: ")
      if(i == 0) println(axs1)
      else println(axs4)

      print("(" +i +")" +"after loop: ")
      if (i==0) println(axs2)
      else println(axs5)

      print("(" +i +")" +"after widen: ")
      if(i==0) println(axs3)
      else println(axs6)
      print("axs for next iteration:")
      axs4 = axs6
      println(axs4)
      println("")

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

    assert(b.widen_AList(c,g).equals(b.AMany(d)))
    assert(b.widen_AList(h,c).equals(b.AMany(e)))
    assert(!b.widen_AList(h,c).equals(b.ANil))

    //TODO
    println(b.widen_AList(c,i)) //AMany([-∞;8])
    println(b.widen_AList(i,c))

    println(b.widen_AList(c,j))
    println(b.widen_AList(j,c))

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

    println(b.widen_AList(g,h)) //returns: ACons([1;1],ACons([-∞;2],AMany([5;5])))
    println(b.widen_AList(h,g)) //returns: ACons([1;1],ACons([1;∞],AMany([5;5])))

    val i = b.AMany(f)
    println(b.widen_AList(g,i)) //returns: AMany([1;∞])
    println(b.widen_AList(i,g)) //returns: AMany([1;2])
    println(b.widen_AList(h,i)) //returns: AMany([1;∞])
    println(b.widen_AList(i,h)) //returns: AMany([1;∞])

  }

  test("widen with AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.AMany(c)
    val f = b.AMany(d)

    println(b.widen_AList(e,f)) //returns: AMany([-1;∞])
    println(b.widen_AList(e,e)) //returns: AMany([-1;3])
    println(b.widen_AList(f,e)) //returns: AMany([-∞;8])
  }



  /***********************
   *Tests widen_AOption  *
   ***********************/

  test("widen with ANone"){

  }

  test("widen with ASome"){

  }

  test("widen with AMaybe"){

  }

}
