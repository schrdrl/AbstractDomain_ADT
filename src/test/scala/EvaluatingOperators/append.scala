package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AInt, AList, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class append extends AnyFunSuite {
  test("Append (concrete)") {
    var xs: List[Int] = List()
    xs = xs :+ 1
    xs = xs :+ 2
    xs = xs :+ 3
    println(xs)

  }

  test("Append (built-in method on abstract Domain AList)") {
    //ANil
    var ws : AList = ANil
    ws = ws.append(AInt(1))
    println(ws)
    ws = ws.append(AInt(2))
    println(ws)
    ws = ws.append(AInt(3))
    println(ws+"\n")

    //ACons(_, ANil)
    var xs : AList = ACons(AInt.top, ANil)
    xs = xs.append(AInt.one)
    println(xs)
    xs = xs.append(AInt(2))
    println(xs)
    xs = xs.append(AInt(Some(0), None))
    println(xs+"\n")

    //ACons(_, AMany(_))
    var ys : AList = ACons(AInt.top, AMany(AInt.zero))
    ys = ys.append(AInt.one)
    println(ys)
    ys = ys.append(AInt(2))
    println(ys)
    ys = ys.append(AInt(Some(0), None))
    println(ys+"\n")

    //AMany(_)
    var zs : AList = AMany(AInt.zero)
    zs = zs.append(AInt.one)
    println(zs)
    zs = zs.append(AInt(2))
    println(zs)
    zs = zs.append(AInt(Some(0), None))
    println(zs+"\n")
  }

  test("Append (Test: integration of AList.append into AOp )") {
    val init = AState(Map("n" -> AInt.zero, "xs" -> ANil))
    val test = APred("isNil", "xs")
    val op = ABlock(AAssign("xs", AOp("append", List(AVar("xs"), AVar("n")))), AAssign("n", AOp("+", List(AVar("n"), AConst(AInt.one)))))

    val as1 = op.execute(Set(init))
    println(as1)

    val as2 = op.execute(as1)
    println(as2)

    val as3 = op.execute(as2)
    println(as3)
  }


  test("Append (concrete-loop)") {
    var xs: List[Int] = List(1, 2, 3, 4)
    var ys: List[Int] = List(0)

    while (!xs.isEmpty){
      ys = ys :+ xs.head
      xs = xs.tail
    }
    println(ys)
    assert(xs.isEmpty)
  }


  test("Append (Test: integration of AList.append into AOp and AWhile)"){
    val init = AState(Map("n" -> AInt.zero, "xs" -> ACons(AInt.one,ACons(AInt(2), ACons(AInt(3), ANil))), "ys" -> ACons(AInt.zero, ANil)))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("just", List(AVar("n")))),
      AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))),
      AAssign("xs", AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("just", List(AVar("xs"))))
    )

    val prog = ABlock(AWhile(!test, body, 5), AAssert(test))

    val as1 = prog.execute(as0)
    println(as1)
  }


}
