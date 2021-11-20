
import Abstraction._
import org.scalatest.funsuite.AnyFunSuite





class BasicFunctionsTest extends AnyFunSuite {

  test("Head of ANil"){

    val a = Intervals.Positive
    val b = ALists(a)
    val c = b.aHead(b.ANil)
    assert(c. equals(b.ANone))
  }

  test("Head of AMany"){
    val a = Intervals.Positive
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d =b.AMany(c)

    assert(b.aHead(d).equals(b.aHead(b.AMany(c))))
  }

  test("Head of ACons"){
    val a = Intervals.Positive
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d =b.ACons(c, b.ANil)

    assert(b.aHead(d).equals(b.aHead(b.ACons(c, b.ANil))))
    assert(b.aHead(d).equals(b.aHead(b.ACons(c, b.AMany(c)))))

  }


  test("Tail of ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    println(b.aTail(c))
  }

  test("Tail of ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, d)
    val g = b.ACons(c, e)

    println(b.aTail(f))
    println(b.aTail(g))

  }

  test("Tail of AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.AMany(c)
    println(b.aTail(d))


  }

  test("Tail of nested ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.ANil
    val e = b.ACons(c, d) //ACons([-1,5], ANil) => ANone => passt
    val f = b.ACons(c, e) // ACons([-1,5],ACons([-1,5], ANil)) =>  ASome([-1;5]) => passt
    val g = b.ACons(c, f) // ACons([-1,5],ACons([-1,5],ACons([-1,5], ANil))) => ASome([-1;5]) => passt

    val h = b.intervals.Interval(IntegerVal(5),IntegerVal(12))
    val i = b.ACons(c, b.ACons(h, b.ANil)) //  ASome([5;12]) => passt

    val j = b.ACons(c, b.ACons(h, f)) // ist: AMaybe([-1;∞]) soll: ASome([-1;12])

    println(b.aTail(e))
    println(b.aTail(f))
    println(b.aTail(g))
    println(b.aTail(i))
    println(b.aTail(j))

    val k = b.ACons(h, b.ACons(b.intervals.Interval(IntegerNegInf, IntegerVal(100)),b.ANil))
    println(b.aTail(k))
  }


  test("Widen Function"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(8))
    val e = b.ANil
    val f = b.AMany(c)
    val g = b.AMany(d)
    val h = b.ACons(c,e)
    val i = b.ACons(d,f)

    println(b.widen_AInt(e,f)) //passt AMaybe([-1;5])
    println(b.widen_AInt(e,h)) //passt AMaybe([-1;5])
    println(b.widen_AInt(e,i)) //TODO: hier AMaybe([-∞;8]) sollte ASome([-1, 8]) ->lower bound
    println(b.widen_AInt(f,g)) //TODO: hier AMaybe([-1;∞]) sollte AMaybe([-1, 8]) ->upper bound
    println(b.widen_AInt(h,i)) //TODO hier AMaybe([-1;∞]) sollte ASome([-1, 8]) ->upper bound
    println(b.widen_AInt(g,h)) //TODO hier AMaybe([-1;∞]) sollte AMaybe([-1,8]) ->upper bound



  }


  test("Length ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    println(b.aLength(b.ANil))
  }

  test("Length ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    println(b.aLength(b.ACons(c, b.ANil)))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(8))
    println(b.aLength(b.ACons(c, b.AMany(d))))
  }


  test("Length AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    println(b.aLength(b.AMany(c)))
  }

  //isConcreteElementOf_Int

  test("Integer Is concrete Element of AInt"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(3),IntegerVal(8))
    val e = 2
    println(b.isConcreteElementOf_Int(e,c))
    println(b.isConcreteElementOf_Int(e,d))
    val f = b.intervals.Interval(IntegerVal(0), IntegerInf)
    val g = b.intervals.Interval(IntegerNegInf, IntegerVal(0))
    println(b.isConcreteElementOf_Int(e,f))
    println(b.isConcreteElementOf_Int(e,g))
  }

  /*
  //isConcreteElementOf_List

  test("empty list is concrete Element of ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = Nil
    val d = b.ANil
    println(b.isConcreteElementOf_List(c,d))
  }

  test("empty list is not concrete Element of ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = Nil
    val e = b.ACons(c,b.ANil)
    println(b.isConcreteElementOf_List(d,e))
  }

  test("empty list is concrete Element of AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = Nil
    val e = b.AMany(c)
    println(b.isConcreteElementOf_List(d,e))
  }

  test("List is not concrete Element of ANil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    val d = List(1,2,3,4)
    println(b.isConcreteElementOf_List(d,c))
  }

  test("List is concrete Element of ACons"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = List(1,2,3,4)
    val d = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val e = b.ACons(d, b.ACons(d, b.ANil)) //true
    println(b.isConcreteElementOf_List(c,e))
    val f = b.intervals.Interval(IntegerVal(5),IntegerVal(8))
    val g = b.ACons(d, b.ACons(f, b.ANil))
    println(b.isConcreteElementOf_List(c,g)) //false
  }

  test("List is concrete Element of AMany"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = List(1,2,3,4)
    val d = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val e = b.AMany(d) //true
    println(b.isConcreteElementOf_List(c,e))

  }
*/
  //isConcreteElementOf_Option



}
