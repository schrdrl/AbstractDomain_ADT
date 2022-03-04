package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AInt, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class head extends AnyFunSuite {

//concrete

  test("head (concrete)"){
    var n = 0
    var xs : List[Int] = List(9,7,4)

    while (!xs.isEmpty){  //maybe also n >= 0
      n = xs.head
      println(n)
      xs = xs.tail
    }
    assert (xs.isEmpty)
  }

  test("head - operator"){
    val xs = ACons(AInt(Some(5), Some(10)), ACons(AInt(Some(-3), Some(8)),ACons(AInt(Some(4), Some(4)), ANil)))
    val init = AState(Map("n" -> AInt.zero, "xs" -> xs))

    //first iteration
    var head = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(init))
    var tail = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(init))
    println(head)
    println(tail)
    println("")

    //second iteration
    head = AAssign("n", AOp("head", List(AVar("xs")))).execute(tail)
    tail = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(tail)
    println(head)
    println(tail)
    println("")

    //second iteration
    head = AAssign("n", AOp("head", List(AVar("xs")))).execute(tail)
    tail = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(tail)
    println(head)
    println(tail)
    println("")
  }



  test("head (abstract-ACons)"){
    val init = AState(Map("n" -> AInt.zero, "xs" -> ACons(AInt(Some(0), Some(9)), ACons(AInt(Some(-3), Some(8)), ACons(AInt(Some(4), Some(4)), ANil)))))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("n", AOp("head", List(AVar("xs")))),
      AAssign("xs", AOp("tail", List(AVar("xs"))))
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test)
    )

    val as1 = prog.execute(as0)
    println(as1)
  }



  test("head (abstract-AMany)"){
    val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt(Some(0), None))))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("n", AOp("head", List(AVar("xs")))),
      AAssign("xs", AOp("tail", List(AVar("xs"))))
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test)
    )

    val as1 = prog.execute(as0)
    println(as1)
  }

}
