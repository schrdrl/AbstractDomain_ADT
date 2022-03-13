package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, AIf, AInt, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class abs extends AnyFunSuite {

  //integer values
  test("Abs (concrete:Int)"){
    var i: Int = -1
    println(i)
    i = i.abs
    println(i)
    assert(i >=0 )

    var j = 1
    println(j)
    j = j.abs
    println(j)
    assert(j >=0 )

  }


  test("Abs (built-in method on abstract Domain AInt)") {
    val a = AInt(-5)
    val b = a.abs

    assert(AInt.<=(Some(0), b.lb) && AInt.<=(Some(0), b.ub))

    val c = AInt(5)
    val d = c.abs

    assert(AInt.<=(Some(0), d.lb) && AInt.<=(Some(0), d.ub))
  }


  test("Abs (Tests integration of AInt.abs into AOp)"){
    val test = APred("isPositive", "n")

    val prog = ABlock(AAssign("n", AOp("abs", List(AVar("n")))), AAssert(test))
    val as0 = Set(AState(Map("n"-> AInt(-5))), AState(Map("n"-> AInt.top)))
    val as1 = prog.execute(as0)
    println(as1)
  }


  test("Abs (Tests integration of AInt.abs into AWhile)"){
    val as0 = Set(AState(Map("n"-> AInt(-5))), AState(Map("n"-> AInt.top)))

    val test = APred("isPositive", "n")
    val body = ABlock(AAssign("n", AOp("abs", List(AVar("n")))))
    val prog = ABlock(AWhile(!test, body, 5), AAssert(test))

    val as1 = prog.execute(as0)
    println(as1)
  }


//Lists containing integer values
  test ("Abs(concrete:List[Int]) -append") {
    var xs: List[Int] = List(1, -3, 10, -22)
    var ys: List[Int] = List()
    println(xs)

    while (!xs.isEmpty) {
      ys = ys :+ xs.head.abs
      println(ys)
      xs = xs.tail
    }
    assert(xs.isEmpty)

    //ys = ys.reverse
    while (!ys.isEmpty) {
      if(ys.head >= 0) xs = ys.head :: xs
      ys = ys.tail
    }
    assert(ys.isEmpty)
    println(xs)
  }



  test ("Abs: (Test: integration of AInt.abs into AWhile/append: AList") {
    val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt.top), "ys" -> ANil))
    val as0 = Set(init)

    var test = APred("isNil", "xs")
    val test_elem = APred("isPositive", "n")

    //apply abs on list-element
    var body = ABlock(
      AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("get", List(AVar("n")))),      //save xs.head in n
      AAssign("n", AOp("abs", List(AVar("n")))),                                                  //n.abs
      AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))),                                  //ys.append(n)
      AAssign("xs", AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("get", List(AVar("xs"))))  //xs.tail
    )

    var prog = ABlock(AWhile(!test, body, 5), AAssert(test))

    val as1 = prog.execute(as0)
    println(as1)


    //test_elem >= 0
    test = APred("isNil", "ys")
    body = ABlock(AAssign("n",AOp("head", List(AVar("ys")))),AAssign("n",AOp("get", List(AVar("n")))),     //save ys.head in n
                  AIf(test_elem, AAssign("xs", AOp("append", List(AVar("xs"), AVar("n"))))),                //if (n >= 0) xs.append(n)
                  AAssign("ys", AOp("tail", List(AVar("ys")))), AAssign("ys",AOp("get", List(AVar("ys")))) //ys.tail
    )
    prog = ABlock(AWhile(!test, body, 5), AAssert(test))
    val as2 = prog.execute(as1)
    println(as2)
  }



  //Lists containing integer values
  test ("Abs(concrete:List[Int] - prepend)") {
    var xs: List[Int] = List(1, -3, 10, -22)
    var ys: List[Int] = List()
    println(xs)

    while (!xs.isEmpty) {
      ys = xs.head.abs :: ys
      println(ys)
      xs = xs.tail
    }
    assert(xs.isEmpty)

    //ys = ys.reverse
    while (!ys.isEmpty) {
      if(ys.head >= 0) xs = ys.head :: xs
      ys = ys.tail
    }
    assert(ys.isEmpty)
    println(xs)
  }




  test ("Abs: (Test: integration of AInt.abs into AWhile/prepend: AList") {
    val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt.top), "ys" -> ANil))
    val as0 = Set(init)

    var test = APred("isNil", "xs")
    val test_elem = APred("isPositive", "n")

    //apply abs on list-element
    var body = ABlock(
      AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("get", List(AVar("n")))),      //save xs.head in n
      AAssign("n", AOp("abs", List(AVar("n")))),                                                  //n.abs
      AAssign("ys", AOp("prepend", List(AVar("ys"), AVar("n")))),                                  //ys.append(n)
      AAssign("xs", AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("get", List(AVar("xs"))))  //xs.tail
    )

    var prog = ABlock(AWhile(!test, body, 5), AAssert(test))

    val as1 = prog.execute(as0)
    println(as1)


    //test_elem >= 0
    test = APred("isNil", "ys")
    body = ABlock(AAssign("n",AOp("head", List(AVar("ys")))),AAssign("n",AOp("get", List(AVar("n")))),     //save ys.head in n
      AIf(test_elem, AAssign("xs", AOp("append", List(AVar("xs"), AVar("n"))))),                //if (n >= 0) xs.append(n)
      AAssign("ys", AOp("tail", List(AVar("ys")))), AAssign("ys",AOp("get", List(AVar("ys")))) //ys.tail
    )
    prog = ABlock(AWhile(!test, body, 5), AAssert(test))
    val as2 = prog.execute(as1)
    println(as2)
  }


}
