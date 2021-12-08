import Abstraction.{ALists, IntegerVal, Intervals}
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

    val e = b.ACons(c, b.ACons(c, b.AMany(c)))
    val f = b.ACons(c, b.ACons(c, b.AMany(d)))
    val g = b.ACons(c, b.ACons(c, b.ANil))
    val h = b.ACons(c, b.ACons(d, b.AMany(c)))

    val i = b.ANil
    val j = b.AMany(c)
    val k = b.AMany(d)
    val l = b.ACons(c, b.ANil)
    val m = b.ACons(d, b.AMany(c))
    val n = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil)))
    val o = b.ACons(c, b.ACons(c, b.ACons(c, b.AMany(c))))
    val p = b.ACons(c, b.ACons(d, b.ACons(c, b.ANil)))
    val q = b.ACons(c, b.ACons(c, b.ACons(d, b.ANil)))

    assert(!b.subset_AList(e,i)) // -> false
    assert(!b.subset_AList(f,i)) // -> false
    assert(b.subset_AList(e,g)) // -> true
    assert(b.subset_AList(g,e)) // -> true
    assert(!b.subset_AList(e,h)) // -> false
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

    //TODO recheck
    println(b.subset_AList(e,n))
    println(b.subset_AList(f,n))
    println(b.subset_AList(g,n))
    println(b.subset_AList(h,n))

    println(b.subset_AList(e,o))
    println(b.subset_AList(f,o))
    println(b.subset_AList(g,o))
    println(b.subset_AList(h,o))

    println(b.subset_AList(e,p))
    println(b.subset_AList(f,p))
    println(b.subset_AList(g,p))
    println(b.subset_AList(h,p))

    println(b.subset_AList(e,q)) // -> false
    println(b.subset_AList(f,q)) // ???
    println(b.subset_AList(g,q)) // ???
    println(b.subset_AList(h,q)) // -> false
  }


}
