package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, AIf, AInt, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite


class abs extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("Abs (built-in method(Scala))"){
    //test with negative Int
    var i: Int = -1
    println(i)
    i = i.abs
    println(i)
    assert(i >=0 )

    //test with positive Int
    var j = 1
    println(j)
    j = j.abs
    println(j)
    assert(j >=0 )
  }

  //1b. Abstract value (AInt) + built-in method (AInt)
  test("Abs (built-in method (abstract domain))") {
    //test with negative AInt
    val a = AInt(-1)
    val h1 = a.hasConcreteElement(-1)
    assert(h1 == true)

    val b = a.abs
    val h2 = b.hasConcreteElement(1)
    val h3 = !b.hasConcreteElement(-1)
    assert(h2 == true && h3 == true)

    assert(AInt.zero.<=(b))

    //test with positive AInt
    val c = AInt(1)
    val h4 = c.hasConcreteElement(1)
    assert(h4 == true)

    val d = c.abs
    val h5 = b.hasConcreteElement(1)
    val h6 = !b.hasConcreteElement(-1)
    assert(h5 == true && h6 == true)

    assert(AInt.zero.<=(d))
  }

  //1c. Abstract value (AInt) + AOp
  test("Abs (integration of AInt.abs into AOp)"){
    val test = APred("isPositive", "n")

    val prog = ABlock(AAssign("n", AOp("abs", List(AVar("n")))), AAssert(test))
    val as0 = Set(AState(Map("n"-> AInt(-1))), AState(Map("n"-> AInt(1))), AState(Map("n"-> AInt.top)))
    val as1 = prog.execute(as0)
    println(as1)
  }



  //2a. Applying abs on all elements of a list
  test ("Abs (applying abs on elements of a list)") {
    var xs: List[Int] = List(1, -3, 10, -22)
    var temp: List[Int] = List()
    println(xs)

    //applying abs on all elements of xs
    while (!xs.isEmpty) {
      temp = xs.head.abs +: temp
      xs = xs.tail
    }
    assert(xs.isEmpty)

    //verifying all elements are positive
    while (!temp.isEmpty) {
      if(temp.head >= 0) xs = temp.head +: xs
      temp = temp.tail
    }
    assert(temp.isEmpty)
    println(xs)
  }



  //2b. Applying abs on all elements of an AList
  test ("Abs: (applying abs on elements of an AList)") {
    val h1 = AMany(AInt.top).hasConcreteElement(List(1, -3, 10, -22))
    assert(h1 == true)

    val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt.top), "ys" -> ANil))
    val as0 = Set(init)
    println("init: " +init)

    var test = APred("isNil", "xs")
    val test_elem = APred("isPositive", "n")

    //apply abs on list-element
    var body = ABlock(
      AAssign("n",AOp("head", List(AVar("xs")))), AAssign("n",AOp("get", List(AVar("n")))),         //save xs.head in n
      AAssign("n", AOp("abs", List(AVar("n")))),                                                    //n.abs
      AAssign("ys", AOp("prepend", List(AVar("ys"), AVar("n")))),                                   //ys.append(n)
      AAssign("xs", AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("get", List(AVar("xs"))))      //xs.tail
    )

    var prog = ABlock(AWhile(!test, body, 5), AAssert(test))  //assert(xs.isEmpty)

    val as1 = prog.execute(as0)



    //test whether all elements of the AList value are positive
    test = APred("isNil", "ys")
    body = ABlock(AAssign("n",AOp("head", List(AVar("ys")))),AAssign("n",AOp("get", List(AVar("n")))),     //save ys.head in n
      AIf(test_elem, AAssign("xs", AOp("append", List(AVar("xs"), AVar("n"))))),                           //if (n >= 0) xs.append(n)
      AAssign("ys", AOp("tail", List(AVar("ys")))), AAssign("ys",AOp("get", List(AVar("ys"))))             //ys.tail
    )

    prog = ABlock(AWhile(!test, body, 5), AAssert(test))  //assert(ys.isEmpty)

    val as2 = prog.execute(as1)
    for(a <- as2){
      val h2 = a.lookup("xs").hasConcreteElement(List(1, 3, 10, 22))
      assert(h2 == true)
      println("out: " +a)
    }
  }


}
