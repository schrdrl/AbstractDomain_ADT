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



  //tail ANil
  //tail ACons
  //tail AMany

  //length ANil
  //length ACons
  //length AMany

  //isConcreteElementOf_Int

  //isConcreteElementOf_List

  //isConcreteElementOf_Option



}
