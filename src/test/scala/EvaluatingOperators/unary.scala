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
    var i: AInt = AInt.one
    println (i)
    i = i.unary_-()
    println (i)
    i = i.unary_-()
    println (i)
  }

  test ("Unary: AInt - AExpr") {
    val init = AState(Map("n"-> AInt.one))

    val op = AOp("-", List(AVar("n"))).evaluate(init)
    println(op)

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

  test ("Unary: AList ") {
    val xs : AList = ACons(AInt.one, ACons(AInt(Some(-5), Some(0)),ACons(AInt(Some(0), Some(115)),ACons(AInt.apply(-22), ANil))))
    println(xs)
    val init = AState(Map("n" -> AInt.top, "xs" -> xs, "ys" -> ANil))

    var test = APred("isNil", "xs")

    var ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(init))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(init))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(init))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(init))
    println(ys)

    //second iteration
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)

    //third iteration
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)

    //fourth iteration
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("-", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)

    AAssert(test).execute(Set(ys.head))

    test = APred("isNil", "ys")
    AAssert(!test).execute(Set(ys.head))
  }

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

