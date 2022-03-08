package EvaluatingOperators

import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AInt, AList, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite


class unary extends AnyFunSuite {

  //integer value
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

  test("Unary (built-in method on abstract Domain AInt)") {
    val a = AInt(-5)
    val b = a.unary_-()

    assert(AInt.<=(Some(0), b.lb) && AInt.<=(Some(0), b.ub))

    val c = AInt(5)
    val d = c.unary_-()

    assert(AInt.<=(d.lb, Some(0)) && AInt.<=(d.ub, Some(0)))
  }


  test("Unary (Tests integration of AInt.unary into AOp)"){
    var test = APred("isPositive", "n")

    var prog = ABlock(AAssign("n", AOp("-", List(AVar("n")))), AAssert(test))
    val as0 = Set(AState(Map("n"-> AInt(-5))), AState(Map("n"-> AInt(None,Some(-1))))) //negative values
    val as1 = Set(AState(Map("n"-> AInt(5))), AState(Map("n"-> AInt(Some(1), None)))) //positive values

    val as2 = prog.execute(as0)
    println(as2)

    test = APred("isNegative", "n")
    prog = ABlock(AAssign("n", AOp("-", List(AVar("n")))), AAssert(test))
    val as3 = prog.execute(as1)
    println(as3)
  }


  test ("Unary:List") {
    var xs: List[Int] = List(1, -3, 10, -22)
    println(xs)
    var ys: List[Int] = List()
    while (!xs.isEmpty) {
      ys = -xs.head :: ys
      xs = xs.tail
    }
    //ys = ys.reverse
    while (!ys.isEmpty) {
      xs = ys.head :: xs
      ys = ys.tail
    }
    assert(ys.isEmpty)
    println(xs)
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

