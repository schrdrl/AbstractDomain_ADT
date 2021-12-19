import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class EqualityTests extends AnyFunSuite {

  test("=== on ABool"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.ATrue
    val d = b.AFalse
    val e = b.AUnknown

    println(a.===(c,c))
    println(a.===(c,c))
  }

}
