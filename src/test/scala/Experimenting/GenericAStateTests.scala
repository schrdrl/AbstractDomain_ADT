package Experimenting
import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class GenericAStateTests extends AnyFunSuite {

  test("General functionality"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val d = b.ANone
    val e = b.ATrue
    val f = b.ACons(c, b.ANil)

    val g = b.AState1(c,d)
    val h = b.AState1(c,e)
    val i = b.AState1(c,f)
    val j = b.AState1(d,e)

    println(g)
    println(h)
    println(i)
    println(j)

    val k = b.AssignN0_test2
    println(k.execute(Set(i)))
    println(k.execute(Set(g)))
    println(k.execute(Set(j)))
    println(k.execute(Set(g,h,i,j)))



  }

}
