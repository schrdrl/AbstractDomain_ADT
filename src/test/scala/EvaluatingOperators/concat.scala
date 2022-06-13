package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, AAssume, ABlock, ACons, AConst, AIf, AInt, AMany, ANil, AOp, APred, ASkip, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class concat extends AnyFunSuite {
  //not a lot of hasConcreteElement tests -> already multiple ones in the other tests

  //1a. Functionality of concat
  test("concat: Functionality"){
    // non-empty list values
    var xs: List[Int] = List(-4, -3, -2, -1)
    var ys: List[Int] = List(0,1,2,3)

    //appending every element of ys to xs
    while (!ys.isEmpty) {
      xs = xs :+ ys.head
      ys = ys.tail
    }

    assert(ys.isEmpty)
    assert(!xs.isEmpty)
  }

  //1b. Concrete values + built-in method (Scala)
  test("concat: built-in method Scala"){
    //empty list values
    assert(List().concat(List())           == List())

    //non-empty list values
    assert(List(1,2,3).concat(List(4,5,6)) == List(1,2,3,4,5,6))

    //empty and non-empty list values
    assert(List(1,2,3).concat(List())      == List(1,2,3))
    assert(List().concat(List(4,5,6))      == List(4,5,6))
  }



  /**
   * 1c. Abstract value + built-in method (Scala) -> illustrated
   *      - To illustrate that the principle is the same as in example 1b
   *      - additional: widen, fixpoint iteration
   */
  test("concat: abstract illustrated") {
    //lists to be concatenated
    val xs = ACons(AInt(-2), ACons(AInt(-1), ANil))
    val ys = ACons(AInt(0), ACons(AInt(1),AMany(AInt(3))))

    //initial state
    val init = AState(Map("n" -> AInt.zero,"xs" -> xs,"ys" -> ys))
    val as0 = Set(init)

    //test condition
    val test = APred("isNil", "ys")

    //extracting the head-element of ys and appending it to xs
    val body = ABlock(
      AAssign("n", AOp("head", List(AVar("ys")))),
      AAssign("n", AOp("get", List(AVar("n")))),
      AAssign("xs", AOp("append", List(AVar("xs"), AVar("n")))),
      AAssign("ys", AOp("tail", List(AVar("ys")))),
      AAssign("ys", AOp("get", List(AVar("ys")))))

    //Program execution
    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test),
      AAssert(!APred("isNil", "xs"))).execute(as0)

    assert(prog.head.lookup("ys") == ANil)
    assert(prog.head.lookup("xs") == ACons(AInt(-2), ACons(AInt(-1), AMany(AInt(Some(0), None)))))
  }

  //1d. Abstract value (AList) + built-in method (AList)
  test("concat: built-in method abstract domain") {
    //concat: (ANil, ANil) -> ANil
    assert(ANil.concat(ANil)                             == ANil)

    //concat: (ANil, ACons) -> ACons
    assert(ANil.concat(ACons(AInt(1), AMany(AInt.top)))  == ACons(AInt(1), AMany(AInt.top)))

    //concat: (ACons, ANil) -> ACons
    assert(ACons(AInt(1), AMany(AInt.top)).concat(ANil) == ACons(AInt(1), AMany(AInt.top)))

    //concat: (ANil, AMany) -> AMany
    assert(ANil.concat(AMany(AInt.top))                  ==  AMany(AInt.top))

    //concat: (AMany, ANil) -> AMany
    assert(AMany(AInt.top).concat(ANil)                  ==  AMany(AInt.top))

    //concat: (ACons, AMany) -> AMany
    assert(ACons(AInt(1), ANil).concat(AMany(AInt(2)))   == ACons(AInt(1), AMany(AInt(2))))

    //concat: (AMany, ACons) -> AMany
    assert(AMany(AInt(1)).concat(ACons(AInt(2), ANil))   == AMany(AInt(Some(1), Some(2))))

    //concat: (AMany, AMany) -> AMany
    assert(AMany(AInt(1)).concat(AMany(AInt(2)))         == AMany(AInt(Some(1), Some(2))))

    //concat: (ACons, ACons) -> ACons
    assert(ACons(AInt(1), ANil).concat(ACons(AInt(2), ANil))
      == ACons(AInt(1), ACons(AInt(2), ANil)))

    assert(ACons(AInt(1), AMany(AInt(3))).concat(ACons(AInt(2), ANil))
      == ACons(AInt(1), AMany(AInt(Some(2), Some(3)))))
  }

  //1e. Abstract value (AList) + AOp
  test("concat: integration into AOp"){
    //initial state
    val a = AState(Map("xs" -> ACons(AInt(None, Some(0)), ANil),
      "ys" -> AMany(AInt(None, Some(0))),
      "zs"-> ANil))

    //Concatenating the values of xs and ys and assigning it to zs
    val op = AAssign("zs", AOp("concat", List(AVar("xs"), AVar("ys"))))

    //Execution of the expression
    val as1 = op.execute(Set(a))

    assert(as1.head.lookup("zs") == ACons(AInt(None, Some(0)), AMany(AInt(None, Some(0)))))
  }


  //1f. concatenate two lists with positive elements, check whether the output list also contains only positive elements
  test("concat: positive elements"){
    //initial state
    val init = AState(Map("n" -> AInt.zero,
      "xs" -> AMany(AInt(Some(2), Some(10))),
      "xs_temp" -> ANil,
      "ys" -> AMany(AInt(Some(5), Some(1999))),
      "ys_temp" -> ANil))

    //Assumption: AMany is not ANil for xs and ys
    val assmptn1 = AAssume(!APred("isNil", "xs")).execute(Set(init))
    val assmptn2 = AAssume(!APred("isNil", "ys")).execute(assmptn1)

    //test condition
    var test = APred("isNil", "xs")

    //Test whether all elements of the given list xs are positive
    var body = ABlock(AAssign("n", AOp("head", List(AVar("xs")))),
      AIf(APred("isSome", "n"), AAssign("n", AOp("get", List(AVar("n"))))),
      AAssert(APred("isPositive", "n")),
      AAssign("xs_temp", AOp("append", List(AVar("xs_temp"), AVar("n")))),
      AAssign("xs",AOp("tail", List(AVar("xs")))),
      AAssign("xs",AOp("get", List(AVar("xs")))))

    //testing whether all elements of xs are positive
    var prog = ABlock(AWhile(!test, body, 5), AAssert(test))
    val as1 = prog.execute(assmptn2)

    //Adjusting the loop condition and the loop body
    test = APred("isNil", "ys")

    //Testing whether all elements of the given list ys are positive
    body = ABlock(AAssign("n", AOp("head", List(AVar("ys")))),
      AIf(APred("isSome", "n"), AAssign("n", AOp("get", List(AVar("n"))))),
      AAssert(APred("isPositive", "n")),
      AAssign("ys_temp", AOp("append", List(AVar("ys_temp"), AVar("n")))),
      AAssign("ys",AOp("tail", List(AVar("ys")))),
      AAssign("ys",AOp("get", List(AVar("ys")))))

    //testing whether all elements of ys are positive
    prog = ABlock(AWhile(!test, body, 5), AAssert(test))
    val as2 = prog.execute(as1)

    //concatenating the lists xs_temp and ys_temp that only contain the positive elements
    val as3 = AAssign("xs", AOp("concat", List(AVar("xs_temp"), AVar("ys_temp")))).execute(as2)

    //Adjusting the loop condition and the loop body
    test = APred("isNil", "xs")

    //Testing whether all elements of the given list ys are positive
    body = ABlock(AAssign("n", AOp("head", List(AVar("xs")))),
      AIf(APred("isSome", "n"), AAssign("n", AOp("get", List(AVar("n"))))),
      AAssert(APred("isPositive", "n")),
      AAssign("ys", AOp("append", List(AVar("ys"), AVar("n")))),
      AAssign("xs",AOp("tail", List(AVar("xs")))),
      AAssign("xs",AOp("get", List(AVar("xs")))))

    //checking whether the output list also contains only positive elements
    val as4 = prog.execute(as3)
  }

}
