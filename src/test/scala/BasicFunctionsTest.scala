
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
    val e = b.ACons(c, d) //ACons([-1,5], ANil) => ANone
    val f = b.ACons(c, e) // ACons([-1,5],ACons([-1,5], ANil)) =>
    val g = b.ACons(c, f) // ACons([-1,5],ACons([-1,5],ACons([-1,5], ANil)))

    val h = b.intervals.Interval(IntegerVal(5),IntegerVal(12))
    val i = b.ACons(c, b.ACons(h, b.ANil))

    val j = b.ACons(c, b.ACons(h, f)) //TODO upper bound
    println(b.aTail(e))
    println(b.aTail(f))
    println(b.aTail(g))
    println(b.aTail(i))
    println(b.aTail(j))

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
  //length ANil
  //length ACons
  //length AMany

  //isConcreteElementOf_Int

  //isConcreteElementOf_List

  //isConcreteElementOf_Option



}
