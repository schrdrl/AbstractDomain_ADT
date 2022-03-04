package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, AConst, AExpr, AInt, AList, AMany, AOp, APred, AState, AVal, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class length extends AnyFunSuite {
  test("length is positive (concrete)"){
    var n = 0
    var xs : List[Int] = List(9,7,4,5)  //only positive numbers

    while (!xs.isEmpty){
      xs = xs.tail
      n = n + 1
    }
    println(n)
    assert (n >= 0)
  }


  test("length is positive (abstract)"){
    val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt(Some(0), None))))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("xs", AOp("tail", List(AVar("xs")))),
      AAssign("n", AOp("+", List(AVar("n"), AConst(AInt.one))))
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(APred("isPositive", "n")) ,
      AAssert(test)
    )

    val as1 = prog.execute(as0)
    println(as1.head.lookup("n"))

    println(as1)
  }



  test("length is positive (operator)"){
    val init = AState(Map("xs" -> AMany(AInt(Some(0), None))))

    val op = AOp("length", List(AVar("xs")))
    val as1 = op.evaluate(init)

    println(as1)


  }

}
