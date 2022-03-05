package EvaluatingOperators

import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AInt, AList, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite


class unary extends AnyFunSuite {

  test ("Unary: Int") {
    var i: Int = - 1
    println (i)
    i = -i
    println (i)

    var j = 1
    println(j)
    j = -j
    println(j)
  }

  test ("Unary: AInt") {
    val test = APred("isPositive", "n")
    var init = AState(Map("n"-> AInt(-5)))
    println(init)
    init = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(init)).head
    println(init)
    var as1 = AAssert(test).execute(Set(init))
    println(as1+"\n")

    init = AState(Map("n"-> AInt(1)))
    println(init)
    init = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(init)).head
    println(init)
    as1 = AAssert(!test).execute(Set(init))
    println(as1+"\n")

    init = AState(Map("n"-> AInt(None, Some(-1))))
    println(init)
    init = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(init)).head
    println(init)
    as1 = AAssert(test).execute(Set(init))
    println(as1+"\n")
  }

  //TODO
  test ("Unary: AInt - AStmt") {
    var init = AState(Map("n"-> AInt(-5)))
    println(init)

    val as0 = Set(init)

    val test = APred("isPositive", "n")

    val body = ABlock(
      AAssign("n", AOp("-", List(AVar("n"))))
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test)
    )

    val as1 = prog.execute(as0)
    println(as1)

  }


  test ("Unary:List") {
    var xs: List[Int] = List(1, -3, 10, -22)
    println(xs)
    var ys: List[Int] = List()
    while (!xs.isEmpty) {
      ys = -xs.head :: ys
      xs = xs.tail
    }
    assert(xs.isEmpty)
    ys = ys.reverse
    println(ys)

  }

  test ("AbsoluteValue: AList ") {
    val test_elem = APred("isPositive", "n")
    var test = APred("isNil", "xs")


    val xs : AList = ACons(AInt.one, ACons(AInt(Some(-5), Some(0)),ACons(AInt(Some(25), Some(115)),ACons(AInt.apply(-22), ANil))))
    println(xs)
    val init = AState(Map("n" -> AInt.zero, "xs" -> xs, "ys" -> ANil))


    println("first iteration")
    var ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(init))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(init))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(init))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(init))
    println(ys)
    var as1 = AAssert(test_elem).execute(ys)
    println(as1 +"\n")

    //second iteration
    println("second iteration")
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))).execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)
    as1 = AAssert(test_elem).execute(ys)
    println(as1 +"\n")


    //third iteration
    println("third iteration")
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)
    as1 = AAssert(!test_elem).execute(ys)
    println(as1 +"\n")



    //fourth iteration
    println("fourth iteration")
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)
    as1 = AAssert(test_elem).execute(ys)
    println(as1 +"\n")


    AAssert(test).execute(Set(ys.head))

    test = APred("isNil", "ys")
    AAssert(!test).execute(Set(ys.head))
  }






//TODO
  test ("Unary: AList - AExpr") {
    val xs : AList = ACons(AInt.one, ACons(AInt(Some(-5), Some(0)),ACons(AInt(Some(0), Some(115)),ACons(AInt.apply(-22), ANil))))
    println(xs)
    val init = AState(Map("n" -> AInt.top, "xs" -> xs, "ys" -> ANil))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("n", AOp("head", List(AVar("xs")))),  //head
      AAssign("n", AOp("-", List(AVar("n")))),      //unary
      AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))), //append
      AAssign("xs", AOp("tail", List(AVar("xs")))) //tail
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test)
    )

    val as1 = prog.execute(as0)
    println(as1)
  }

}

