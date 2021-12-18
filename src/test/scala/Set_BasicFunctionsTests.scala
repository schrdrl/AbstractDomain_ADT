import AList.{IntegerVal, Intervals}
import AList_Set._
import org.scalatest.funsuite.AnyFunSuite
class Set_BasicFunctionsTests extends AnyFunSuite {

  test("aHead"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ANil
    println(b.aHead(Set(c)))
  }


  test("isNil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.ANil
    val e = b.AMany(c)
    val f = b.ACons(c, b.ANil)

    println(b.isNil(Set(d)))
    println(b.isNil(Set(e))) //TODO gibt nur ATrue aus
    println(b.isNil(Set(f)))
  }
}
