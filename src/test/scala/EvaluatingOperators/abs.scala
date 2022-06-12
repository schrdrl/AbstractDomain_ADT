package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, AAssume, ABlock, ACons, AIf, AInt, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite


class abs extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("abs: built-in method Scala"){
    //test with negative Int
    var i: Int = -1
    println(i)
    i = i.abs
    assert(i == 1)
    assert(i >=0 )

    //test with positive Int
    var j = 1
    println(j)
    j = j.abs
    assert(j == 1)
    assert(j >=0 )

    //test with 0
    var k = 0
    k = k.abs
    assert(k == 0)
    assert(k >=0 )
  }

  //1b. Abstract value (AInt) + built-in method (AInt)
  test("abs: built-in method abstract domain") {
    //test with negative AInt
    val a = AInt(-1)
    val h1 = a.hasConcreteElement(-1)
    assert(h1)

    val b = a.abs
    val h2 = b.hasConcreteElement(1)
    val h3 = !b.hasConcreteElement(-1)
    assert(h2 && h3)

    assert(AInt.zero.<=(b))

    //test with positive AInt
    val c = AInt(1)
    val h4 = c.hasConcreteElement(1)
    assert(h4)

    val d = c.abs
    val h5 = d.hasConcreteElement(1)
    val h6 = !d.hasConcreteElement(-1)
    assert(h5 && h6)

    assert(AInt.zero.<=(d))

    //test with any AInt
    val e = AInt.top
    val h7 = e.hasConcreteElement(1)
    val h8 = e.hasConcreteElement(-1)
    assert(h7 && h8)

    val f = c.abs
    val h9 = f.hasConcreteElement(1)
    val h10 = !f.hasConcreteElement(-1)
    assert(h9 && h10)

    assert(AInt.zero.<=(f))

    //further tests
    assert(AInt(0).abs                  == AInt(0))
    assert(AInt(1).abs                  == AInt(1))
    assert(AInt(-1).abs                 == AInt(1))
    assert(AInt(Some(-1), Some(1)).abs  == AInt(1))
    assert(AInt(Some(-5), Some(-1)).abs == AInt(Some(1), Some(5)))
    assert(AInt(Some(4), None).abs      == AInt(Some(4), None))
    assert(AInt(None, Some(17)).abs     == AInt(Some(17), None))
    assert(AInt(None, None).abs         == AInt(Some(0), None))
  }

  //1c. Abstract value (AInt) + AOp
  test("abs: integration of AInt.abs into AOp"){
    //initial state
    val as0 = Set(AState(Map("n"-> AInt.top)))

    //test condition
    val test = APred("isPositive", "n")

    //program body
    val prog = ABlock(
      AAssign("n", AOp("abs", List(AVar("n")))),
      AAssert(test)).execute(as0)

    assert(prog.head.lookup("n") == AInt(Some(0),None))
  }


  //2a. Applying abs on all elements of a list
  test ("abs: applying abs on elements of a list") {
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
    assert(xs == List(1,3,10,22))
    println(xs)
  }



  //2b. Applying abs on all elements of an AList
  test ("abs: applying abs on elements of an AList") {
    //initial state
    val init = AState(Map("n" -> AInt.zero,
      "xs" -> AMany(AInt.top), "temp" -> ANil))

    //Test condition used during the loop execution
    var test = APred("isNil", "xs")

    //Test condition applied on the elements
    val test_elem = APred("isPositive", "n")

    //1. apply abs on the elements of the list
    var body = ABlock(
      //determine the current head-element
      AAssign("n",AOp("head", List(AVar("xs")))),  AAssign("n",AOp("get", List(AVar("n")))),
      //apply abs on this element
      AAssign("n", AOp("abs", List(AVar("n")))),
      //assign the converted element to temp
      AAssign("temp", AOp("prepend",List(AVar("temp"), AVar("n")))),
      //reassign xs with xs.tail
      AAssign("xs", AOp("tail", List(AVar("xs")))),
      AAssign("xs",AOp("get", List(AVar("xs")))))

    //execution of the loop and asserting that the list xs is empty
    var prog = ABlock(AWhile(!test, body, 5), AAssert(test))
    val as1 = prog.execute(Set(init))

    //Adjusting the loop condition
    test = APred("isNil", "temp")

    //2. test whether all elements of the list value are positive
    body = ABlock(
      //determine the current head-element
      AAssign("n",AOp("head", List(AVar("temp")))), AAssign("n",AOp("get", List(AVar("n")))),
      //Verify that tis element is a positive value;
      //if it fulfills this condition, append it to xs
      AIf(test_elem, AAssign("xs", AOp("append", List(AVar("xs"), AVar("n"))))),
      //reassign temp with temp.tail
      AAssign("temp", AOp("tail", List(AVar("temp")))),
      AAssign("temp",AOp("get", List(AVar("temp")))))


    //execution of the loop and asserting that the temporary list is empty
    prog = ABlock(AWhile(!test, body, 5), AAssert(test))
    val as2 = prog.execute(as1)
    }
}
