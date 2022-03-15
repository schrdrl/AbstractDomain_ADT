package EvaluatingOperators

import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AExpr, AIf, AInt, AList, AMany, ANil, AOp, APred, AState, AVal, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class length extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("head (built-in method (Scala))") {

    //test with empty-list
    val xs: List[Int] = List()
    val length_xs = xs.length
    println(length_xs)
    assert(0 <= length_xs)

    //test with non-empty list
    val ys: List[Int] = List(9, 7, 4, 5)
    val length_ys = ys.length
    println(length_ys)
    assert(0 <= length_ys)
  }


  /**
   * 1b. Concrete value + built-in method (Scala) -> illustrated
   *    To illustrate how the built-in length method in Scala works
   *    and how the built-in method in the abstract domain of AList
   *    covers them all
   */
  test("length (concrete: illustrated)") {
    var n = 0 //init: length of the empty list
    var xs: List[Int] = List(-5, 9, 7, -10, 4, 5, -1)
    while (!xs.isEmpty) {
      xs = xs.tail
      n = n + 1
    }
    println(n)  //returns 7
    assert(n >= 0)
  }


  /**
   * 1c. Abstract value + built-in method (Scala) -> illustrated
   *      - To illustrate that the principle is the same as in example 1b
   *      - additional: widen, fixpoint iteration
   */
  test("length (abstract: illustrated)") {
    val xs = ACons(AInt(-5), ACons(AInt(9), ACons(AInt(7), ACons(AInt(-10), AMany(AInt.top)))))
    val h1 = xs.hasConcreteElement(List(-5, 9, 7, -10, 4, 5, -1))
    assert(h1)

    val init = AState(Map("n" -> AInt.zero, "xs" -> xs))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("xs", AOp("tail", List(AVar("xs")))),
      AAssign("xs", AOp("get", List(AVar("xs")))),
      AAssign("n", AOp("+", List(AVar("n"), AConst(AInt.one))))
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(APred("isPositive", "n")) //assert n is positive
    )

    val as1 = prog.execute(as0)
    for(a <- as1) {
      assert(a.lookup("n").hasConcreteElement(7)) // -> return value of test 1c
      println("out: "+a)
    }

  }


  //1d. Abstract value (AList) + built-in method (AList)
  test("length (built-in method (abstract domain))") {
    //test with ANil
    val xs = ANil

    var length = xs.length
    println(length)

    val test = APred("isPositive", "n")
    var state = AState(Map("n"-> length))

    assert(length == AInt.zero)

    //test with ACons
    val ys = ACons(AInt.top, AMany(AInt.top))

    val h1 = ys.hasConcreteElement(List(9, 7, 4, 5))
    val h2 = ys.hasConcreteElement(List(-5, 9, 7, -10, 4, 5, -1))
    val h3 = !ys.hasConcreteElement(List())
    assert(h1 && h2 && h3)

    length = ys.length
    println(length)

    state = AState(Map("n"-> length))

    AAssert(test).execute(Set(state)) //not assert(AInt[0; Inf) <= length) -> can't compare the intervals like that


    //test with AMany
    val zs = AMany(AInt.top)

    val h4 = zs.hasConcreteElement(List(9, 7, 4, 5))
    val h5 = zs.hasConcreteElement(List(-5, 9, 7, -10, 4, 5, -1))
    val h6 = zs.hasConcreteElement(List())
    assert(h4 && h5 && h6)

    length = zs.length
    println(length)

    state = AState(Map("n"-> length))

    AAssert(test).execute(Set(state))
  }



  //1e. Abstract value (AList) + AOp
  test("length (integration into AOp)") {
    val xs = ACons(AInt(Some(5), Some(10)), ACons(AInt(Some(-3), Some(8)), ACons(AInt(Some(4), Some(4)), ANil)))
    val as0 = Set(AState(Map("n" -> AInt.top, "xs" -> xs)), AState(Map("n"-> AInt.top, "xs" -> ANil)) , AState(Map("n"-> AInt.top, "xs" -> AMany(AInt.top))))

    val test = APred("isPositive", "n")

    val prog = ABlock(
      AAssign("n", AOp("length", List(AVar("xs")))),
      AAssert(test)
    )

    val as1 = prog.execute(as0)
    for(a <- as1) println("out: "+a)
  }
}
