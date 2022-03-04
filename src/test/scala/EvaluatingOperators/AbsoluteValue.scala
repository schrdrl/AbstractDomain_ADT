package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AInt, AList, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class AbsoluteValue extends AnyFunSuite {

  test("Absolute value (concrete)"){
    var i: Int = -1
    println(i)
    i = i.abs
    println(i)

    var j = 1
    println(j)
    j = j.abs
    println(j)
  }

  test("Absolute value (abstract)"){
    var i: AInt = AInt.apply(-1)
    println(i)
    i = i.abs
    println(i)

    var j = AInt.one
    println(j)
    j = j.abs
    println(j)

    var k: AInt = AInt(None, Some(-1))
    println(k)
    k = k.abs
    println(k)
  }

  test ("Absolute value:List") {
    var xs: List[Int] = List(1, -3, 10, -22)
    println(xs)
    var ys: List[Int] = List()
    while (!xs.isEmpty) {
      ys = xs.head.abs :: ys
      xs = xs.tail
    }
    assert(xs.isEmpty)
    ys = ys.reverse
    println(ys)
  }


  test ("AbsoluteValue: AList ") {
    val xs : AList = ACons(AInt.one, ACons(AInt(Some(-5), Some(0)),ACons(AInt(Some(0), Some(115)),ACons(AInt.apply(-22), ANil))))
    println(xs)
    val init = AState(Map("n" -> AInt.top, "xs" -> xs, "ys" -> ANil))

    var test = APred("isNil", "xs")

    var ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(init))
    ys = AAssign("n", AOp("abs", List(AVar("n")))).execute(Set(init))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(init))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(init))
    println(ys)

    //second iteration
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("abs", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)

    //third iteration
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("abs", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)

    //fourth iteration
    ys = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(ys.head))
    ys = AAssign("n", AOp("abs", List(AVar("n")))).execute(Set(ys.head))
    ys = AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))) .execute(Set(ys.head))
    ys = AAssign("xs", AOp("tail", List(AVar("xs")))).execute(Set(ys.head))
    println(ys)

    AAssert(test).execute(Set(ys.head))

    test = APred("isNil", "ys")
    AAssert(!test).execute(Set(ys.head))
  }

  //TODO
  test ("AbsoluteValue: AList - AExpr") {
    val xs : AList = ACons(AInt.one, ACons(AInt(Some(-5), Some(0)),ACons(AInt(Some(0), Some(115)),ACons(AInt.apply(-22), ANil))))
    println(xs)
    val init = AState(Map("n" -> AInt.top, "xs" -> xs, "ys" -> ANil))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("n", AOp("head", List(AVar("xs")))),  //head
      AAssign("n", AOp("abs", List(AVar("n")))),      //abs
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
