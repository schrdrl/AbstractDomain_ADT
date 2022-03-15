package EvaluatingOperators

import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AIf, AInt, AList, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite


class unary extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("unary (built-in method(Scala))"){
    //test with negative Int
    var i: Int = -1
    println(i)
    i = i.unary_-
    println(i)
    assert(i >=0 )

    //test with positive Int
    var j = 1
    println(j)
    j = j.unary_-
    println(j)
    assert(j <=0 )
  }

  //1b. Abstract value (AInt) + built-in method (AInt)
  test("unary (built-in method (abstract domain))") {
    //test with negative AInt
    val a = AInt(-1)
    val h1 = a.hasConcreteElement(-1)
    assert(h1)

    val b = a.unary_-
    val h2 = b.hasConcreteElement(1)
    val h3 = !b.hasConcreteElement(-1)
    assert(h2 && h3)

    assert(AInt.zero.<=(b))

    //test with positive AInt
    val c = AInt(1)
    val h4 = c.hasConcreteElement(1)
    assert(h4)

    val d = c.unary_-
    val h5 = b.hasConcreteElement(1)
    val h6 = !b.hasConcreteElement(-1)
    assert(h5 && h6)

    assert(AInt.zero.<=(d))
  }


  //1c. Abstract value (AInt) + AOp
  test("unary (integration of AInt.unary into AOp)"){

    val prog = AAssign("n", AOp("-", List(AVar("n"))))
    val as0 = Set(AState(Map("n"-> AInt(-5))), AState(Map("n"-> AInt(1))), AState(Map("n"-> AInt.top)))
    for (a <- as0) println("init: " +a)

    val as1 = prog.execute(as0)
    for (a <- as1) println("out: " +a)
  }

}

