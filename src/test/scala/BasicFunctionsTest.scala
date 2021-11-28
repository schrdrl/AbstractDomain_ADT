
import Abstraction._
import org.scalatest.funsuite.AnyFunSuite


class BasicFunctionsTest extends AnyFunSuite {

 /***************
  *Tests aHead  *
  **************/
  test("Head of ANil"){

    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.aHead(b.ANil)
    assert(c.equals(b.ANone))
  }

  test("Head of AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.AMany(c)
    assert(b.aHead(d).equals(b.AMaybe(c)))
  }

  test("Head of ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.ACons(c, b.ANil)
    val e = b.ACons(c, b.AMany(c))
    assert(b.aHead(d).equals(b.ASome(c)))
    assert(b.aHead(d).equals(b.aHead(e)))
  }

  /***************
   *Tests aTail  *
   **************/
  test("Tail of ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    assert(b.aTail(c).equals(b.ANone))
  }

  test("Tail of ACons (not nested)"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, d) //ACons([-1,5], ANil)
    val g = b.ACons(c, e) //ACons([-1,5], AMany([-1,5]))


    assert(b.aTail(f).equals(b.ASome(b.ANil)))
    assert(!b.aTail(f).equals(b.ANone)) //not ANil
    assert(b.aTail(g).equals(b.ASome(b.AMany(c))))
    assert(!b.aTail(g).equals(b.ANone)) //not ANil
  }

  test("Tail of AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.AMany(c)
    assert(b.aTail(d).equals(b.AMaybe(b.AMany(c))))
  }

  test("Tail of nested ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(5),IntegerVal(12))
    val e = b.intervals.Interval(IntegerNegInf, IntegerVal(100))

    val g = b.ANil
    val h = b.ACons(c, g) //ACons([-1,5], ANil)
    val i = b.ACons(c, h) // ACons([-1,5],ACons([-1,5], ANil))
    val j = b.ACons(c, i) // ACons([-1,5],ACons([-1,5],ACons([-1,5], ANil)))

    val k = b.ACons(c, b.ACons(d,g)) // ACons([-1,5], ACons([5,12], ANil))
    val l = b.ACons(c, b.ACons(d,h)) //ACons([-1,5], ACons([5,12], ACons([-1,5], ANil)))
    val m = b.ACons(d, b.ACons(e, k)) //ACons([5, 12], ACons([-∞, 100],ACons([-1,5], ACons([5,12], ANil))))

    val n = b.ACons(d, b.ANil)
    val o = b.ACons(d, h)
    val p = b.ACons(e, k)

    assert(b.aTail(h).equals(b.ASome(b.ANil)))
    assert(b.aTail(i).equals(b.ASome(h)))
    assert(b.aTail(j).equals(b.ASome(i)))
    assert(b.aTail(k).equals(b.ASome(n)))
    assert(b.aTail(l).equals(b.ASome(o)))
    assert(b.aTail(m).equals(b.ASome(p)))

  }

  /***************
   *Tests aLength  *
   **************/
  test("Length ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    assert(b.aLength(c).equals(b.ANone))
  }

  test("Length ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(1), IntegerInf)

    val f = b.ACons(c, b.ANil)
    val g = b.ACons(c, b.AMany(d))

    assert(b.aLength(f).equals(b.ASome(e)))
    assert(b.aLength(g).equals(b.ASome(e)))
  }


  test("Length AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(0),IntegerInf)
    val e = b.AMany(c)
    assert(b.aLength(e).equals(b.ASome(d)))
  }

  /**********************************
   *Tests: isConcreteElementOf_Int  *
   **********************************/


  test("Integer is concrete element of AInt"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(8))
    val e = b.intervals.Interval(IntegerVal(0), IntegerInf)
    val f = b.intervals.Interval(IntegerNegInf, IntegerVal(0))

    val g = 2
    val h = -2

    assert(b.isConcreteElementOf_Int(g,c))
    assert(!b.isConcreteElementOf_Int(g,d))
    assert(b.isConcreteElementOf_Int(g,e))
    assert(!b.isConcreteElementOf_Int(g,f))

    assert(!b.isConcreteElementOf_Int(h,c))
    assert(!b.isConcreteElementOf_Int(h,d))
    assert(!b.isConcreteElementOf_Int(h,e))
    assert(b.isConcreteElementOf_Int(h,f))
  }

  /**********************************
   *Tests: isConcreteElementOf_List *
   **********************************/

  test("Empty list is concrete element of ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = Nil
    val d = b.ANil
    assert(b.isConcreteElementOf_List(c,d))
  }

  test("Empty list is not concrete element of ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = Nil
    val e = b.ACons(c,b.ANil)
    assert(!b.isConcreteElementOf_List(d,e))
  }

  test("Empty list is concrete element of AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = List()
    val e = b.AMany(c)
    assert(b.isConcreteElementOf_List(d,e))
  }

  test("Non-empty list is not concrete element of ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    val d = List(1,2,3,4)
    assert(!b.isConcreteElementOf_List(d,c))
  }

  test("Non-empty list is concrete element of ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = List(1,2,3,4)
    //Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    //ACons([1,1], ACons([2,2], ACons([3,3], ACons([4,4], ANil))))

    val d = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val e = b.intervals.Interval(IntegerVal(5),IntegerVal(8))

    val i1 = b.intervals.Interval(IntegerVal(1), IntegerVal(1))
    val i2 = b.intervals.Interval(IntegerVal(2), IntegerVal(2))
    val i3 = b.intervals.Interval(IntegerVal(3), IntegerVal(3))
    val i4 = b.intervals.Interval(IntegerVal(4), IntegerVal(4))

    val f = b.ACons(d, b.ACons(d, b.ANil)) // ACons([-1,5], ACons([-1,5], ANil))
    val g = b.ACons(d, b.ACons(e, b.ANil)) //ACons([5,8], ANil)
    val h = b.ACons(d, b.ACons(e, f)) //ACons([5,8], ACons([-1,5], ACons([-1,5], ANil)))
    val k = b.ACons(i1, b.ACons(i2, b.ACons(i3, b.ACons(i4, b.ANil))))

    assert(!b.isConcreteElementOf_List(c,f))
    assert(!b.isConcreteElementOf_List(c,g))
    assert(!b.isConcreteElementOf_List(c,h))
    assert(b.isConcreteElementOf_List(c,k))

  }

  test("Non-empty list is concrete element of AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = List(1,2,3,4)

    val d = b.intervals.Interval(IntegerVal(-1),IntegerVal(4))
    val e = b.intervals.Interval(IntegerVal(-1),IntegerVal(3))
    val f = b.intervals.Interval(IntegerVal(5),IntegerVal(8))

    val g = b.AMany(d)
    val h = b.AMany(e)
    val i = b.AMany(f)

    assert(b.isConcreteElementOf_List(c,g))
    assert(!b.isConcreteElementOf_List(c,h))
    assert(!b.isConcreteElementOf_List(c,i))

  }

  /**********************************
   *Tests: isConcreteElementOf_Option *
   **********************************/
  test("None is concrete element of ANone"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    assert(b.isConcreteElementOf_Option(None, b.ANone))
  }

  test("None is not concrete element of ASome"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(4))
    assert(!b.isConcreteElementOf_Option(None, b.ASome(c)))
  }

  test("None is concrete element of AMaybe"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(4))
    println(b.isConcreteElementOf_Option(None, b.AMaybe(c)))
  }

  test("Some is not concrete element of ANone"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    assert(!b.isConcreteElementOf_Option(Some(1), b.ANone))
  }

  test("Some is concrete element of ASome"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(4))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(4))

    val e = b.ASome(c)
    val f = b.ASome(d)

    assert(b.isConcreteElementOf_Option(Some(1), e))
    assert(!b.isConcreteElementOf_Option(Some(1), f))
  }

  test("Some is concrete Element of AMaybe"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(4))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(4))

    val e = b.AMaybe(c)
    val f = b.AMaybe(d)

    assert(b.isConcreteElementOf_Option(Some(1), e))
    assert(!b.isConcreteElementOf_Option(Some(1), f))
  }



}
