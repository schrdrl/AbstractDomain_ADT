package EvaluatingOperators

import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AExpr, AIf, AInt, AList, AMany, ANil, ANone, AOp, APred, AState, AVal, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class length extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("length: built-in method Scala") {
    //length operation applied on an empty list
    val xs = List()
    assert(xs.length == 0)
    assert(0 <= xs.length)

    //length operation applied on a non-empty list
    val ys = List(9, 7, 4, 5)
    assert(ys.length == 4)
    assert(0 <= ys.length)
  }


  /**
   * 1b. Concrete value + built-in method (Scala) -> illustrated
   *    To illustrate how the built-in length method in Scala works
   *    and how the built-in method in the abstract domain of AList
   *    covers them all
   */
  test("length: concrete execution (as illustrated)") {
    var n = 0 //init: length of the empty list
    var xs: List[Int] = List(-5, 9, 7, -10, 4, 5, -1)
    while (!xs.isEmpty) {
      xs = xs.tail
      n = n + 1
    }
    assert(n == 7)  //returns 7
    assert(n >= 0)
  }


  /**
   * 1c. Abstract value + built-in method (Scala) -> illustrated
   *      - To illustrate that the principle is the same as in example 1b
   *      - additional: widen, fixpoint iteration
   */
  test("length: abstract execution (as illustrated)") {
    //initial state
    val as0 = Set(AState(Map("n" -> AInt.zero,
      "xs" -> ACons(AInt(-5), ACons(AInt(9), AMany(AInt.top))))))

    //test condition
    val test = APred("isNil", "xs")

    //While body assigning the tail of the list to xs and increasing n by [1,1]
    val body = ABlock(
      AAssign("xs", AOp("tail", List(AVar("xs")))),
      AAssign("xs", AOp("get", List(AVar("xs")))),
      AAssign("n", AOp("+",List(AVar("n"), AConst(AInt.one)))))

    //Execution of the while loop and asserting length (*$>=$*) 0
    val prog = ABlock(
      AWhile(!test, body, 5),
      //assert n is positive
      AAssert(APred("isPositive", "n"))).execute(as0)

    println(prog)

    assert(prog.head.lookup("n") == AInt(Some(0),None))
    assert(prog.head.lookup("xs") == ANil)
  }


  //1d. Abstract value (AList) + built-in method (AList)
  test("length: built-in method abstract domain") {
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

    //further tests
    assert(ANil.length                                 == AInt(0))
    assert(ACons(AInt(1), ANil).length                 == AInt(Some(1),None))
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).length == AInt(Some(1),None))
    assert(ACons(AInt(1), AMany(AInt(2))).length       == AInt(Some(1),None))
    assert(AMany(AInt(0)).length                       == AInt(Some(0),None))
    assert(AMany(AInt(None, Some(5))).length           == AInt(Some(0),None))
  }



  //1e. Abstract value (AList) + AOp
  test("length: integration into AOp") {
    //length used in an expression
    val op = AOp("length", List(AVar("xs")))

    //assigning the length of shape ACons as expression to variable a
    val a = op.evaluate(AState(Map("xs"-> ACons(AInt(7), AMany(AInt(4))))))
    assert(a == AInt(Some(1), None))

    //assigning the length of shape ANil as expression to variable b
    val b = op.evaluate(AState(Map("xs"-> ANil)))
    assert(b == AInt(0))

    //assigning the length of shape AMany as expression to variable c
    val c = op.evaluate(AState(Map("xs"-> AMany(AInt.top))))
    assert(c == AInt(Some(0), None))
  }

  //1e. Abstract value (AList) + AWhile
  test("length: integration into AStmt") {
    //initial state
    val as0 = Set(AState(Map("n"-> AInt.zero,
      "xs" -> AMany(AInt.top))))

    //Program body
    val prog = ABlock(
      AAssign("n", AOp("length", List(AVar("xs")))),
      AAssert(APred("isPositive", "n"))).execute(as0)

    assert(prog.head.lookup("n") == AInt(Some(0),None))
  }




}
