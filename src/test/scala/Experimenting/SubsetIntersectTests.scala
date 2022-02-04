package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class SubsetIntersectTests extends AnyFunSuite {

  /***********
   * Subset  *
   ***********/

  test("ANil is subset of any AList"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ANil
    val f = b.AMany(c)
    val g = b.ACons(d, b.ANil)
    val h = b.ACons(d, b.AMany(c))

    assert(b.subset_AList(e,e))
    assert(b.subset_AList(e,f))
    assert(b.subset_AList(e,g))
    assert(b.subset_AList(e,h))
  }

  test("Subset: AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(10), IntegerVal(18))

    val f = b.AMany(c) //AMany[-1,3]
    val g = b.ANil  // ->true
    val h = b.AMany(d) //AMany[5,8] -> false
    val i = b.ACons(c, b.ANil) //ACons([-1,3], ANil) -> true
    val j = b.ACons(c, b.AMany(d)) //ACons([-1,3], AMany[5,8]) -> false
    val k = b.ACons(e, b.AMany(e)) //ACons([10,18], AMany[10,18]) -> false
    val l = b.ACons(c, b.AMany(e)) //ACons([10,18], AMany[10,18]) -> false
    val m = b.ACons(c, b.AMany(c)) //ACons([-1,3], AMany[-1,3]) -> true

    assert(b.subset_AList(f,g))
    assert(!b.subset_AList(f,h))
    assert(b.subset_AList(f,i))
    assert(!b.subset_AList(f,j))
    assert(!b.subset_AList(f,k))
    assert(!b.subset_AList(f,l))
    assert(b.subset_AList(f,m))
  }

  test("Subset: ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(10), IntegerVal(18))

    val f = b.ACons(c, b.ANil) //ACons([-1,3], ANil)
    val g = b.ACons(c, b.AMany(c)) //ACons([-1,3], AMany([-1,3]))
    val h = b.ACons(c, b.AMany(d)) //ACons([-1,3], AMany([5,8]))
    val i = b.ANil
    val j = b.AMany(c) //AMany([-1,3]
    val k = b.AMany(d) //AMany([5,8]
    val l = b.AMany(e) //AMany([10,18]

    assert(b.subset_AList(f,f)) //->true
    assert(b.subset_AList(f,g)) //->true
    assert(b.subset_AList(g,f)) //->true
    assert(b.subset_AList(f,h)) //-> true
    assert(b.subset_AList(h,f)) //-> true
    assert(!b.subset_AList(g,h)) //-> false
    assert(!b.subset_AList(h,g)) //-> false
    assert(!b.subset_AList(f,i)) //-> false
    assert(!b.subset_AList(g,i)) //-> false
    assert(!b.subset_AList(h,i)) //-> false
    assert(b.subset_AList(f,j)) //-> true
    assert(b.subset_AList(g,j)) //-> true
    assert(!b.subset_AList(h,j)) //-> false
    assert(!b.subset_AList(f,k)) //-> false
    assert(!b.subset_AList(g,k)) //-> false
    assert(!b.subset_AList(h,k)) //-> false
    assert(!b.subset_AList(f,l)) //-> false
    assert(!b.subset_AList(g,l)) //-> false
    assert(!b.subset_AList(h,l)) //-> false
  }

  test("Subset: Nested ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ACons(c, b.ACons(c, b.AMany(c))) //ACons(c, ACons(c, AMany(c)))
    val f = b.ACons(c, b.ACons(c, b.AMany(d))) //ACons(c, ACons(c, AMany(d)))
    val g = b.ACons(c, b.ACons(c, b.ANil))      //ACons(c, ACons(c, ANil))
    val h = b.ACons(c, b.ACons(d, b.AMany(c)))  //ACons(c, ACons(d, AMany(c)))

    val i = b.ANil
    val j = b.AMany(c)
    val k = b.AMany(d)
    val l = b.ACons(c, b.ANil)
    val m = b.ACons(d, b.AMany(c))
    val n = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil))) //ACons(c, ACons(c, ACons(c, ANil)))
    val o = b.ACons(c, b.ACons(c, b.ACons(c, b.AMany(c)))) //ACons(c, ACons(c, ACons(c, AMany(c))))
    val p = b.ACons(c, b.ACons(d, b.ACons(c, b.ANil))) // ACons(c, ACons(d, ACons(c, ANil)))
    val q = b.ACons(c, b.ACons(c, b.ACons(d, b.ANil))) //ACons(c, ACons(c, ACons(d, ANil)))

    assert(!b.subset_AList(e,i)) // -> false
    assert(!b.subset_AList(f,i)) // -> false
    assert(b.subset_AList(e,g)) // -> true
    assert(b.subset_AList(g,e)) // -> true

    assert(b.subset_AList(e,j)) // -> true
    assert(!b.subset_AList(f,j)) // -> false
    assert(b.subset_AList(g,j)) // -> true
    assert(!b.subset_AList(h,j)) // -> false

    assert(!b.subset_AList(e,k)) // -> false
    assert(!b.subset_AList(e,h)) // -> false

    assert(!b.subset_AList(e,l)) // -> false
    assert(!b.subset_AList(f,l)) // -> false
    assert(!b.subset_AList(g,l)) // -> false
    assert(!b.subset_AList(h,l)) // -> false

    assert(!b.subset_AList(e,m)) // -> false
    assert(!b.subset_AList(f,m)) // -> false
    assert(!b.subset_AList(g,m)) // -> false
    assert(!b.subset_AList(h,m)) // -> false

    assert(b.subset_AList(e,n))  // -> true
    assert(!b.subset_AList(f,n)) // -> false
    assert(b.subset_AList(g,n))  // -> true
    assert(!b.subset_AList(h,n)) // -> false

    assert(b.subset_AList(e,o))  // -> true
    assert(!b.subset_AList(f,o)) // -> false
    assert(b.subset_AList(g,o)) // -> true
    assert(!b.subset_AList(h,o)) // -> false

    assert(!b.subset_AList(e,p)) // -> false
    assert(!b.subset_AList(f,p)) // -> false
    assert(!b.subset_AList(g,p)) // -> false
    assert(b.subset_AList(h,p)) // -> true

    assert(!b.subset_AList(e,q)) // -> false
    assert(b.subset_AList(f,q)) // -> true
    assert(b.subset_AList(g,q)) // -> true
    assert(!b.subset_AList(h,q)) // -> false
  }

  /*****************
   * Intersection  *
   *****************/

  test("Intersect with ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))

    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, b.ANil)

    assert(b.intersect_AList(d, d) == b.ANil)
    assert(b.intersect_AList(d, e) == b.ANil)
    assert(b.intersect_AList(e, d) ==  b.intersect_AList(d, e))
    assert(b.intersect_AList(d, f) == b.ANil)
    assert(b.intersect_AList(f, d) == b.intersect_AList(d, f))
  }

  test("Intersect with AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.AMany(c)
    val f = b.AMany(d)
    val g = b.ANil
    val h = b.ACons(c, b.ANil)
    val i = b.ACons(c, b.AMany(c))
    val j = b.ACons(c, b.AMany(d))

    assert(b.intersect_AList(e,e) == b.AMany(c))
    assert(b.intersect_AList(e,f) == b.ANil)
    assert(b.intersect_AList(f,e) == b.ANil)
    assert(b.intersect_AList(e,g) == b.ANil)
    assert(b.intersect_AList(e,h) == h)
    assert(b.intersect_AList(e,i) == i)
    assert(b.intersect_AList(e,j) == h)
  }


  test("Intersect with ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ACons(c, b.ANil)
    val f = b.ACons(c, b.AMany(c))
    val g = b.ACons(d, b.AMany(c))

    val h = b.ANil
    val i = b.AMany(c)
    val j = b.AMany(d)

    assert(b.intersect_AList(e,e) == e)
    assert(b.intersect_AList(e,f) == e)
    assert(b.intersect_AList(f,e) == b.intersect_AList(e,f))
    assert(b.intersect_AList(e,g) == b.ANil)
    assert(b.intersect_AList(e,h) == b.ANil)
    assert(b.intersect_AList(e,i) == e)
    assert(b.intersect_AList(e,j) == b.ANil)
    assert(b.intersect_AList(g,j) == b.ACons(d, b.ANil))
  }


  test("Intersect with nested ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ACons(c, b.ACons(c, b.AMany(c)))
    val f = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil)))
    val g = b.ACons(c, b.ACons(d, b.ACons(c,b.ANil)))

    val h = b.ACons(c, b.ANil)
    val i = b.ACons(c, b.AMany(c))
    val j = b.ACons(d, b.AMany(c))

    val k = b.ANil
    val l = b.AMany(c)
    val m = b.AMany(d)

    assert(b.intersect_AList(e,e) == e)
    assert(b.intersect_AList(e,f) == f)
    assert(b.intersect_AList(f,e) == b.intersect_AList(e,f))
    assert(b.intersect_AList(e,g) == h)
    assert(b.intersect_AList(h,g) == b.intersect_AList(g,h))
    assert(b.intersect_AList(g,h) == h)
    assert(b.intersect_AList(e,i) == e)
    assert(b.intersect_AList(f,i) == f)
    assert(b.intersect_AList(g,j) == b.ANil)
    assert(b.intersect_AList(f,k) == b.ANil)
    assert(b.intersect_AList(e,l) == e)
    assert(b.intersect_AList(e,m) == b.ANil)
    assert(b.intersect_AList(g,m) == b.ANil)
  }

  test("Intersection AOption[AInt]"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ANone
    val f = b.ASome(c)
    val g = b.AMaybe(c)
    val h = b.ASome(d)
    val i = b.AMaybe(d)

    println(b.intersect_AOption_AInt(f,f))
    println(b.intersect_AOption_AInt(e,f)) //None, ASome
    println(b.intersect_AOption_AInt(e,g))  //None, AMaybe
    println(b.intersect_AOption_AInt(f,g)) //ASome, AMaybe
    println(b.intersect_AOption_AInt(h,f))  //ASome, ASome
    println(b.intersect_AOption_AInt(g,i)) //AMaybe, AMaybe
    println("")

    val j = b.intervals.Interval(IntegerVal(0), IntegerVal(6))
    val k = b.ASome(j)
    val l = b.AMaybe(j)

    println(b.intersect_AOption_AInt(k,l))
    println(b.intersect_AOption_AInt(f,l))
    println(b.intersect_AOption_AInt(f,k))
    println(b.intersect_AOption_AInt(i,l))
    println(b.intersect_AOption_AInt(i,k))
  }

  test("Intersection AOption[AList]"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(5), IntegerVal(8))

    val e = b.ANone
    val f = b.ASome(b.ANil)
    val g = b.AMaybe(b.ANil)
    val h = b.ASome(b.AMany(c))
    val i = b.AMaybe(b.AMany(c))

    val l = b.ASome(b.ACons(c, b.ANil))
    val m = b.AMaybe(b.ACons(c, b.AMany(c)))
    val n = b.AMaybe(b.ACons(c, b.AMany(d)))
    val o = b.ASome(b.ACons(c, b.ACons(c, b.ANil)))
    val p = b.AMaybe(b.ACons(c, b.ACons(d, b.ANil)))

    println(b.intersect_AOption_AList(e,f)) //ANone, ASome(ANil)
    println(b.intersect_AOption_AList(e,g)) //ANone, AMaybe(ANil)
    println(b.intersect_AOption_AList(f,h)) //ASome(ANil), ASome(AMany)
    println(b.intersect_AOption_AList(h,i)) //ASome(AMany), AMaybe(AMany)
    println("")

    println(b.intersect_AOption_AList(l,m)) //ASome(ACons), AMaybe(ACons)
    println(b.intersect_AOption_AList(m,n)) //AMaybe(ACons), AMaybe(ACons)
    println(b.intersect_AOption_AList(m,o)) //AMaybe(ACons), ASome(ACons)
    println(b.intersect_AOption_AList(o,p)) //ASome(ACons), AMaybe(ACons)
    println(b.intersect_AOption_AList(i,p)) //AMaybe(AMany), AMaybe(ACons)

  }


}
