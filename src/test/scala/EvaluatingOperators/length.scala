package EvaluatingOperators

import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AExpr, AIf, AInt, AList, AMany, ANil, AOp, APred, AState, AVal, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class length extends AnyFunSuite {

  //1.
  test("length: concrete (built-in method)") {
    val xs: List[Int] = List(9, 7, 4, 5)
    val length = xs.length
    println(length)
    assert(0 <= length)
  }


  //2.
  test("length (built-in method on abstract Domain AList)") {
    val xs = ACons(AInt.top, AMany(AInt.top))
    val length = xs.length
    println(length)
    assert(AInt(Some(0), None).<=(length))
  }


  //3.
  test("length (integration into AOp)") {
    val xs = ACons(AInt(Some(5), Some(10)), ACons(AInt(Some(-3), Some(8)), ACons(AInt(Some(4), Some(4)), ANil)))
    val as0 = Set(AState(Map("n" -> AInt.zero, "xs" -> xs)))

    val test = APred("isNil", "xs")
    val test_elem = APred("isPositive", "n")

    val body = ABlock(
      AAssign("n", AOp("length", List(AVar("xs")))) //+
    )

    val prog = ABlock(
      AIf(!test, body),
      AAssert(!test), //APred("isPositive", "n")
      AAssert(test_elem)
    )

    val as1 = prog.execute(as0)
    println(as1)
  }




  //4
  test("length (concrete:counter)") {
    var n = 0
    var xs: List[Int] = List(9, 7, 4, 5) //only positive numbers
    while (!xs.isEmpty) {
      xs = xs.tail
      n = n + 1
    }
    println(n)
    assert(n >= 0)
  }


  //5
  test("length is positive (AOp:counter)") {
    //val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt(Some(0), None))))
    val init = AState(Map("n" -> AInt.zero, "xs" -> ACons(AInt(Some(0), None),ANil)))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("xs", AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("just", List(AVar("xs")))) ,
      AAssign("n", AOp("+", List(AVar("n"), AConst(AInt.one))))
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(APred("isPositive", "n")),
      AAssert(test)
    )

    val as1 = prog.execute(as0)

    println(as1)
  }


}
