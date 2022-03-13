package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AInt, AList, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class prepend extends AnyFunSuite {
  test("Prepend (concrete)") {
    var xs: List[Int] = List()
    xs = 1 :: xs
    xs = 2 :: xs
    xs = 3 :: xs
    println(xs)

  }

  test("Prepend (built-in method on abstract Domain AList)") {
    //ANil
    var ws : AList = ANil
    ws = ws.prepend(AInt(1))
    println(ws)
    ws = ws.prepend(AInt(2))
    println(ws)
    ws = ws.prepend(AInt(3))
    println(ws+"\n")

    //ACons(_, ANil)
    var xs : AList = ACons(AInt.top, ANil)
    xs = xs.prepend(AInt.one)
    println(xs)
    xs = xs.prepend(AInt(2))
    println(xs)
    xs = xs.prepend(AInt(Some(0), None))
    println(xs+"\n")

    //ACons(_, AMany(_))
    var ys : AList = ACons(AInt.top, AMany(AInt.zero))
    ys = ys.prepend(AInt.one)
    println(ys)
    ys = ys.prepend(AInt(2))
    println(ys)
    ys = ys.prepend(AInt(Some(0), None))
    println(ys+"\n")

    //AMany(_)
    var zs : AList = AMany(AInt.zero)
    zs = zs.prepend(AInt.one)
    println(zs)
    zs = zs.prepend(AInt(2))
    println(zs)
    zs = zs.prepend(AInt(Some(0), None))
    println(zs+"\n")
  }

  test("Prepend (Test: integration of AList.prepend into AOp )") {
    val init = AState(Map("n" -> AInt.zero, "xs" -> ANil))
    val test = APred("isNil", "xs")
    val op = ABlock(AAssign("xs", AOp("prepend", List(AVar("xs"), AVar("n")))), AAssign("n", AOp("+", List(AVar("n"), AConst(AInt.one)))))

    //prepend elements
    val as1 = op.execute(Set(init))
    println(as1)

    val as2 = op.execute(as1)
    println(as2)

    val as3 = op.execute(as2)
    println(as3)

    //xs shouldn't be empty
    AAssert(!test).execute(as3)
    println(as3)
  }


  test("Prepend (concrete-loop)") {
    var xs: List[Int] = List(1, 2, 3, 4)
    var ys: List[Int] = List(0)

    while (!xs.isEmpty){
      ys = xs.head :: ys
      xs = xs.tail
    }
    println(ys)
    assert(xs.isEmpty)
    assert(!ys.isEmpty)
  }


  test("Prepend (Test: integration of AList.prepend into AOp and AWhile)"){
    val init = AState(Map("n" -> AInt.zero, "xs" -> ACons(AInt.one,ACons(AInt(2), ACons(AInt(3), ANil))), "ys" -> ACons(AInt.zero, ANil)))
    val as0 = Set(init)

    var test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("get", List(AVar("n")))),
      AAssign("ys", AOp("prepend", List(AVar("ys"), AVar("n")))),
      AAssign("xs", AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("get", List(AVar("xs"))))
    )

    val prog = ABlock(AWhile(!test, body, 5), AAssert(test))

    val as1 = prog.execute(as0)
    println(as1)

    test = APred("isNil", "ys")
    val as2 = AAssert(!test).execute(as1)
    println(as2)
  }


}
