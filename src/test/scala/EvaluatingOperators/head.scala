package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AIf, AInt, AList, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class head extends AnyFunSuite {


  //1
  test("head: concrete value(implemented method)"){
    val xs : List[Int] = List(9,7,4)
    val n = xs.head
    println(n)
  }


  //2
  test("head: absolute value(implemented method)"){
    val xs : AList = ACons(AInt(9), ACons(AInt(7), AMany(AInt(4))))
    val n = xs.head
    println(n)
  }


  //3
  test("head: absolute value(AOp)"){
    val xs : AList = ACons(AInt(9), ACons(AInt(7), AMany(AInt(4))))

    val test = APred("isSome", "n")

    val prog = ABlock(AAssign("n", AOp("head", List(AVar("xs")))), AIf(test, AAssign("n", AOp("just", List(AVar("n"))))))

    val as0 = Set(AState(Map("xs"-> xs, "n" ->AInt.zero)), AState(Map("xs"-> AMany(AInt.top), "n" ->AInt.zero)))
    val as1 = prog.execute(as0)
    println(as1)
  }


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

    val test = APred("isNil", "xs")


    //first iteration
    var head = ABlock(AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("just", List(AVar("n"))))).execute(Set(init))
    var tail = ABlock(AAssign("xs",AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("just", List(AVar("xs"))))).execute(head)
    println(head)
    println(tail)
    println("")

    //second iteration
    head = ABlock(AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("just", List(AVar("n"))))).execute(tail)
    tail = ABlock(AAssign("xs",AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("just", List(AVar("xs"))))).execute(head)
    println(head)
    println(tail)
    println("")

    //second iteration
    head = ABlock(AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("just", List(AVar("n"))))).execute(tail)
    tail = ABlock(AAssign("xs",AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("just", List(AVar("xs"))))).execute(head)
    println(head)
    println(tail)
    println("")

    AAssert(test).execute(tail)
  }


//TODO
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


//TODO
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
