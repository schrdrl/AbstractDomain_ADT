package EvaluatingOperators
import AList_CleanCode.{ACons, AInt, AMany, ANil}
import org.scalatest.funsuite.AnyFunSuite

class reverse extends AnyFunSuite {

  //1.
  test("reverse: concrete(built-in method)"){
    var a : List[Int] = List(1,2,3)
    var b : List[Int] = List(-1,-2,-3)
    var c : List[Int] = List()

    a = a.reverse
    b = b.reverse
    c = c.reverse

    println(a)
    println(b)
    println(c)
  }

  //2.
  test("reverse: abstract(built-in method)"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))
    val g = ACons(c, ACons(b, ANil))

    println(d.reverse())
    println(e.reverse())
    println(f.reverse())
    println(g.reverse())
  }

  //3.
  test("reverse: abstract(integration into AOp)"){

  }
}
