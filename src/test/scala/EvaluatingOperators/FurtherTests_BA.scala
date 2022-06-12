package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, AAssume, ABlock, ACons, AConst, AFalse, AIf, AInt, AMany, AMaybe, ANil, ANone, AOp, APred, ASome, AState, ATrue, AUnknown, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite
class FurtherTests_BA extends AnyFunSuite {

  //1. Introduction
  test("Motivating Example"){
    var n = 0
    var i = 0
    var xs = List(1,2,3,4,5)
    val l = xs.length

    while(n <= l){
      i = i + xs.head
      xs = xs.tail
      n = n + 1
    }
  }


  //4. Abstract Domain
  //4.2. ABool
  test("ABool: Demonstration of implemented operations in ABool"){
    //hasConcreteElement: (AVal, Any) -> Boolean
    assert(ATrue.hasConcreteElement(true)     == true)
    assert(ATrue.hasConcreteElement(false)    == false)
    assert(AFalse.hasConcreteElement(true)    == false)
    assert(AFalse.hasConcreteElement(false)   == true)
    assert(AUnknown.hasConcreteElement(true)  == true)
    assert(AUnknown.hasConcreteElement(false) == true)

    //widen: (AVal, AVal) -> ABool
    assert(ATrue.widen(ATrue)                 == ATrue)
    assert(AFalse.widen(AFalse)               == AFalse)
    assert(AFalse.widen(ATrue)                == AUnknown)
    assert(AUnknown.widen(AFalse)             == AUnknown)
    assert(ATrue.widen(AUnknown)              == AUnknown)

    // eq: (AVal, AVal) -> ABool
    assert(ATrue.eq(ATrue)                    == ATrue)
    assert(AFalse.eq(AFalse)                  == ATrue)
    assert(AFalse.eq(ATrue)                   == AFalse)
    assert(AUnknown.eq(AFalse)                == AUnknown)
    assert(ATrue.eq(AUnknown)                 == AUnknown)

    // noneq: (AVal, AVal) -> ABool
    assert(ATrue.noneq(ATrue)                 == AFalse)
    assert(AFalse.noneq(AFalse)               == AFalse)
    assert(AFalse.noneq(ATrue)                == ATrue)
    assert(AUnknown.noneq(AFalse)             == AUnknown)
    assert(ATrue.noneq(AUnknown)              == AUnknown)

    // &&: (AVal, AVal) -> ABool
    assert(ATrue.&&(ATrue)                    == ATrue)
    assert(AFalse.&&(AFalse)                  == AFalse)
    assert(AFalse.&&(ATrue)                   == AFalse)
    assert(AUnknown.&&(ATrue)                 == AUnknown)
    assert(ATrue.&&(AUnknown)                 == AUnknown)

    // ||: (AVal, AVal) -> ABool
    assert(ATrue.||(ATrue)                    == ATrue)
    assert(AFalse.||(AFalse)                  == AFalse)
    assert(AFalse.||(ATrue)                   == ATrue)
    assert(AUnknown.||(AFalse)                == AUnknown)
    assert(AUnknown.||(ATrue)                 == ATrue)
    assert(AUnknown.||(AUnknown)              == AUnknown)


    // !: AVal -> ABool
    assert(AFalse.!                           == ATrue)
    assert(ATrue.!                            == AFalse)
    assert(AUnknown.!                         == AUnknown)
  }

  //4.3. AOption
  test("AOption: widen, hasConcreteElement"){
    //hasConcreteElement: (AVal, Any) -> Boolean
    assert(ANone.hasConcreteElement(None)                             == true)
    assert(ANone.hasConcreteElement(Some(1))                          == false)

    assert(ASome(AInt(Some(0),Some(5))).hasConcreteElement(None)      == false)
    assert(ASome(AInt(Some(0),Some(5))).hasConcreteElement(Some(-1))  == false)
    assert(ASome(AInt(Some(0),Some(5))).hasConcreteElement(Some(3))   == true)

    assert(AMaybe(AInt(Some(0),Some(5))).hasConcreteElement(None)     == true)
    assert(AMaybe(AInt(Some(0),Some(5))).hasConcreteElement(Some(-1)) == false)
    assert(AMaybe(AInt(Some(0),Some(5))).hasConcreteElement(Some(3))  == true)


    //widen: (AVal, AVal) -> AOption
    assert(ANone.widen(ANone)                     == ANone)
    assert(ANone.widen(ASome(AInt(2)))            == AMaybe(AInt(2)))
    assert(ANone.widen(AMaybe(AInt(2)))           == AMaybe(AInt(2)))

    assert(ASome(AInt(1)).widen(ANone)            == AMaybe(AInt(1)))
    assert(ASome(AInt(1)).widen(ASome(AInt(2)))   == ASome(AInt(Some(1), None)))
    assert(ASome(AInt(1)).widen(AMaybe(AInt(2)))  == AMaybe(AInt(Some(1), None)))

    assert(AMaybe(AInt(1)).widen(ANone)           == AMaybe(AInt(1)))
    assert(AMaybe(AInt(1)).widen(ASome(AInt(2)))  == AMaybe(AInt(Some(1), None)))
    assert(AMaybe(AInt(1)).widen(AMaybe(AInt(2))) == AMaybe(AInt(Some(1), None)))
  }

  //4.4. AInt
  test("AInt: hasConcreteElement"){
    //hasConcreteElement: (AVal, Any) -> Boolean
    assert(AInt(1).hasConcreteElement(1)                 == true)
    assert(AInt(Some(-1), Some(1)).hasConcreteElement(1) == true)
    assert(AInt(Some(-1), Some(5)).hasConcreteElement(1) == true)
    assert(AInt(None, Some(1)).hasConcreteElement(1)     == true)
    assert(AInt(None, Some(100)).hasConcreteElement(1)   == true)
    assert(AInt(None, None).hasConcreteElement(1)        == true)
  }

  test("AInt: widen"){
    //widen: (AVal, Any) -> AInt
    assert(AInt(1).widen(AInt(2))                 == AInt(Some(1), None))
    assert(AInt(1).widen(AInt(2))                 == AInt(Some(1), None))
    assert(AInt(1).widen(AInt(-2))                == AInt(None, Some(1)))
    assert(AInt(1).widen(AInt(Some(-1), Some(5))) == AInt(None, None))
  }

  test("AInt: Further Operations"){
    // +: (AVal, AVal) -> AInt
    assert(AInt(Some(-1), Some(5)).+(AInt(2))         == AInt(Some(1), Some(7)))
    assert(AInt(None, Some(1)).+(AInt(2))             == AInt(None, Some(3)))

    // - : (AVal, AVal) -> AInt
    assert(AInt(Some(-1), Some(5)).-(AInt(2))         == AInt(Some(-3), Some(3)))
    assert(AInt(None, Some(1)).-(AInt(2))             == AInt(None, Some(-1)))

    // * : (AVal, AVal) -> AInt
    assert(AInt(Some(-1), Some(5)).*(AInt(2))         == AInt(Some(-2),Some(10)))
    assert(AInt(None, Some(1)).*(AInt(2))             == AInt(None, Some(2)))

    // / : (AVal, AVal) -> AInt
    assert(AInt(Some(-1), Some(5))./(AInt(2))         == AInt(Some(0), Some(2)))
    assert(AInt(None, Some(1))./(AInt(2))             == AInt(None, Some(0)))

    // abs : AVal -> AInt
    assert(AInt(Some(-1), Some(5)).abs                == AInt(Some(1), Some(5)))
    assert(AInt(None, Some(1)).abs                    == AInt(Some(1), None))

    // unary_- : AVal -> AInt
    assert(AInt(Some(-1), Some(5)).unary_-            == AInt(Some(-5), Some(1)))
    assert(AInt(None, Some(1)).unary_-                == AInt(Some(-1), None))

    //union : (AVal, AVal) -> AInt
    assert(AInt(Some(-1), Some(5)).union(AInt(7))     == AInt(Some(-1), Some(7)))
    assert(AInt(None, Some(1)).union(AInt(7))         == AInt(None, Some(7)))

    //intersect : (AVal, AVal) -> AInt
    assert(AInt(Some(-1), Some(5)).intersect(AInt(7)) == AInt(Some(0), Some(-1)))
    assert(AInt(Some(-1), Some(5)).intersect(AInt(3)) == AInt(3))
    assert(AInt(Some(-1), Some(2)).intersect(AInt(3)) == AInt(Some(0),Some(-1)))

      // comp : (AVal, AVal) -> (Set[AVal], Set[AVal])
      //equal part(s) of the two values
      assert(AInt(Some(-10), Some(5)).comp(AInt(Some(1), Some(7)))._1 == Set(AInt(Some(1), Some(5))))

      // Non-equal Part(s) of the first valuecompared to the second value
      assert(AInt(Some(-10), Some(5)).comp(AInt(Some(1), Some(7)))._2 == Set(AInt(Some(-10), Some(0))))

  }

  //4.5. AList
  test("AList: hasConcreteElement"){
    //hasConcreteElement: (AVal, Any) -> Boolean
    assert(ANil.hasConcreteElement(List())                                      == true)
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).hasConcreteElement(List(1,2))   == true)
    assert(ACons(AInt.top, ACons(AInt.top, ANil)).hasConcreteElement(List(1,2)) == true)
    assert(AMany(AInt(Some(1), Some(2))).hasConcreteElement(List(1,2))          == true)
    assert(AMany(AInt(Some(1),None)).hasConcreteElement(List(1,2))              == true)
    assert(AMany(AInt.top).hasConcreteElement(List(1,2))                        == true)
  }

  test("AList: widen"){
    //widen: (AVal, Val) -> AList

    //widen shapes: ANil, ANil
    assert(ANil.widen(ANil) == ANil)

    //widen shapes: (ANil, ACons) and (ACons, ANil)
    assert(ANil.widen(ACons(AInt(-1), AMany(AInt(7)))) == AMany(AInt(Some(-1),None)))
    assert(ACons(AInt(-1), AMany(AInt(7))).widen(ANil) == AMany(AInt(Some(-1),None)))

    //widen shapes: (ANil, AMany) and (AMany, ANil)
    assert(ANil.widen(AMany(AInt(Some(-1),Some(5))))   == AMany(AInt(Some(-1),Some(5))))
    assert(AMany(AInt(Some(-1),Some(5))).widen(ANil)   == AMany(AInt(Some(-1),Some(5))))

    //widen shapes: ACons ACons
    assert(ACons(AInt(10), AMany(AInt(1))).widen(ACons(AInt(-1), ANil))
      == ACons(AInt(None, Some(10)), AMany(AInt(1))))

    assert(ACons(AInt(-1), ANil).widen(ACons(AInt(10), AMany(AInt(1))))
      == ACons(AInt(Some(-1), None), AMany(AInt(1))))

    //widen shapes: (ACons, AMany) and (AMany, ACons)
    assert(AMany(AInt(7)).widen(ACons(AInt(-1), ANil))  == AMany(AInt(None,Some(7))))
    assert(ACons(AInt(-1), ANil).widen(AMany(AInt(7))) == AMany(AInt(Some(-1),None)))

    //widen shapes: AMany, AMany
    assert(AMany(AInt(7)).widen(AMany(AInt(Some(-1),Some(5)))) == AMany(AInt(None,Some(7))))
    assert(AMany(AInt(Some(-1),Some(5))).widen(AMany(AInt(7))) == AMany(AInt(Some(-1),None)))
  }

  test("AList: further Operation"){
    //headOption: AList -> AOption
    assert(ANil.headOption                                 == ANone)
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).headOption == ASome(AInt(1)))
    assert(AMany(AInt(None, Some(19))).headOption          == AMaybe(AInt(None, Some(19))))

    //tailOption: AList -> AOption
    assert(ANil.tailOption                                 == ANone)
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).tailOption == ASome(ACons(AInt(2), ANil)))
    assert(AMany(AInt(None, Some(19))).tailOption          == AMaybe(AMany(AInt(None, Some(19)))))

    //length: AList -> AInt
    assert(ANil.length                                     == AInt(0))
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).length     == AInt(Some(1), None))
    assert(AMany(AInt(None, Some(19))).length              == AInt(Some(0), None))

    //reverse: AList -> AList
    assert(AMany(AInt(None, Some(19))).reverse             == AMany(AInt(None, Some(19))))
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).reverse
      == ACons(AInt(2), ACons(AInt(1), ANil)))

    //prepend: AList, AVal -> AList
    assert(ANil.prepend(AInt(1))                          == ACons(AInt(1), ANil))
    assert(AMany(AInt(None, Some(19))).prepend(AInt(0))   == AMany(AInt(None, Some(19))))
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).prepend(AInt(0))
      == ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ANil))))

    //concat: AList, AVal -> AList
    assert(ANil.concat(ACons(AInt(1), AMany(AInt(2))))    == ACons(AInt(1), AMany(AInt(2))))
    assert(AMany(AInt(7)).concat(AMany(AInt(10)))         == AMany(AInt(Some(7), Some(10))))
    assert(AMany(AInt(7)).concat(ACons(AInt(1), AMany(AInt(2))))
      == AMany(AInt(Some(1), Some(7))))

    //intersect: AList, AVal -> AList
    assert(ANil.intersect(ACons(AInt(1), AMany(AInt(2)))) == ACons(AInt(Some(0), Some(-1)), ANil))
    assert(ACons(AInt(1), AMany(AInt(2))).intersect(ANil) == ACons(AInt(Some(0), Some(-1)), ANil))
    assert(AMany(AInt(7)).intersect(ANil)                 == ANil)
    assert(AMany(AInt(7)).intersect(ACons(AInt(7),ANil))  == ACons(AInt(7), ANil))
    assert(AMany(AInt(None, Some(19))).intersect(ACons(AInt(1), AMany(AInt(2))))
      == ACons(AInt(1), AMany(AInt(2))))
    assert(AMany(AInt(7)).intersect(AMany(AInt(10)))
      == ACons(AInt(Some(0), Some(-1)), ANil))

    //append: AList, AVal -> AList
    assert(ANil.append(AInt(0))                           == ACons(AInt(0), ANil))
    assert(ACons(AInt(1), ACons(AInt(2), ANil)).append(AInt(0))
      == ACons(AInt(1), ACons(AInt(2), ACons(AInt(0), ANil))))
    assert(AMany(AInt(None, Some(19))).append(AInt(0))    == AMany(AInt(None, Some(19))))
  }


  //5. Representation of Programs

  test("Examples of program instructions"){
    var n = 10             // Program state variable

    n = 10 - 10 + 1        // Reassigning values
    assert(n >= 1)         // Assertions

    if(n != 10) n = 10     // Conditional statements

    while (n >= 0){        // While-loops
      n = n - 1
    }
  }

  //5.1. AState
  test("Program states"){
    var xs = List(1,2,3)  // state: xs =  List(1,2,3)
    var i = 0             // state: xs =  List(1,2,3), i = 0
    var n = 0             // state: xs =  List(1,2,3), i = 0, n = 0

    //updating the variables' values
    i = 5                 // state: xs =  List(1,2,3), i = 5, n = 0
    n = i                 // state: xs =  List(1,2,3), i = 5, n = 5
    xs = xs.tail          // state: xs =  List(2,3),   i = 5, n = 5
  }

  //5.2. AExpr
  test("Demonstration of AExpr"){
    //initial state
    val as0 = AState(Map("xs" -> ACons(AInt(1), ANil),
      "n"  -> AInt(1),
      "ao" -> ASome(ANil),
      "b"  -> AUnknown))

    //Expression "head" determines the value of headOption applied to the program variable xs
    val a = AOp("head", List(AVar("xs"))).evaluate(as0)
    assert(a == ASome(AInt(1)))

    //Expression "+" adds the constant value [1,1] to the value stored in variable n
    val b = AOp("+", List(AVar("n"), AConst(AInt(1)))).evaluate(as0)
    assert(b == AInt(2))

    //Expression "!=" compares the constant value ATrue with the value stored in variable x
    val c = AOp("!=", List(AVar("b"), AConst(ATrue))).evaluate(as0)
    assert(c == AUnknown)

    //Expression "get" returns the option's value stored in variable a0
    val d = AOp("get", List(AVar("ao"))).evaluate(as0)
    assert(d == ANil)
  }

  test("+ operator"){
    //nested expression
    val expr = AOp("+", List(AVar("m"), AVar("n"))).+(AOp("+", List(AVar("n"), AVar("m"))))

    val a = expr.evaluate(AState(Map("n"-> AInt(2), "m"-> AInt(3))))
    assert(a == AInt(10))
  }

  //5.3. ATest
  test("tests for lists"){
    //shape: ANil
    val p_0 = APred("isNil", "xs").positive(AState(Map("xs" -> ANil)))
    val n_0 = APred("isNil", "xs").negative(AState(Map("xs" -> ANil)))
    assert(p_0 == Set(AState(Map("xs" -> ANil))))    //fulfills test
    assert(n_0 == Set())         //does not fulfills test

    //shape: ACons
    val p_1 = APred("isNil", "ys").positive(AState(Map("ys" -> ACons(AInt(1), ANil))))
    val n_1 = APred("isNil", "ys").negative(AState(Map("ys" -> ACons(AInt(1), ANil))))
    assert(p_1 == Set())
    assert(n_1 == Set(AState(Map("ys" -> ACons(AInt(1),ANil)))))

    //shape: AMany
    val p_2 = APred("isNil", "zs").positive(AState(Map("zs" -> AMany(AInt(1)))))
    val n_2 = APred("isNil", "zs").negative(AState(Map("zs" -> AMany(AInt(1)))))
    println(p_2)
    println(n_2)
    assert(p_2 == Set(AState(Map("zs" -> ANil))))
    assert(n_2 == Set(AState(Map("zs" -> ACons(AInt(1),AMany(AInt(1)))))))
  }

  test("numeric tests"){
    //Test with a value containing positive and negative values
    //testing "isPositive"
    val p_0 = APred("isPositive","n").positive(AState(Map("n" -> AInt(Some(-1), Some(5)))))
    val n_0 = APred("isPositive","n").negative(AState(Map("n" -> AInt(Some(-1), Some(5)))))
    assert(p_0 == Set(AState(Map("n" -> AInt(Some(0), Some(5))))))
    assert(n_0 == Set(AState(Map("n" -> AInt(-1)))))

    //testing "isNegative"
    val p_1 = APred("isNegative","n").positive(AState(Map("n" -> AInt(Some(-1), Some(5)))))
    val n_1 = APred("isNegative","n").negative(AState(Map("n" -> AInt(Some(-1), Some(5)))))
    assert(p_1 == Set(AState(Map("n" -> AInt(Some(-1), Some(0))))))
    assert(n_1 == Set(AState(Map("n" -> AInt(Some(1), Some(5))))))


    //Test with a value containing unspecified values from -(*$\infty$*) to a specified positive value
    //testing "isPositive"
    val p_2 = APred("isPositive","m").positive(AState(Map("m" -> AInt(None, Some(10)))))
    val n_2 = APred("isPositive","m").negative(AState(Map("m" -> AInt(None, Some(10)))))
    assert(p_2 == Set(AState(Map("m" -> AInt(Some(0), Some(10))))))
    assert(n_2 == Set(AState(Map("m" -> AInt(None, Some(-1))))))

    //testing "isNegative"
    val p_3 = APred("isNegative","m").positive(AState(Map("m" -> AInt(None, Some(10)))))
    val n_3 = APred("isNegative","m").negative(AState(Map("m" -> AInt(None, Some(10)))))
    assert(p_3 == Set(AState(Map("m" -> AInt(None, Some(0))))))
    assert(n_3 == Set(AState(Map("m" -> AInt(Some(1), Some(10))))))
  }

  test("unary_!"){
    //initial state
    val as0 = AState(Map("xs" -> ACons(AInt(1), ANil), "n"  -> AInt(Some(-1), Some(5))))

    //negating the tests
    val t0 = APred("isNil", "xs").unary_!() //or: !isNil
    val t1 = APred("isPositive", "n").unary_!() //or: !isPositive

    //executing the test
    val p_0 = t0.positive(as0)
    val n_0 = t0.negative(as0)
    assert(p_0 == Set(AState(Map("xs" -> ACons(AInt(1),ANil), "n" -> AInt(Some(-1),Some(5))))))
    assert(n_0 == Set())

    val p_1 = t1.positive(as0)
    val n_1 = t1.negative(as0)
    assert(p_1 == Set(AState(Map("xs" -> ACons(AInt(1),ANil), "n" -> AInt(-1)))))
    assert(n_1 == Set(AState(Map("xs" -> ACons(AInt(1),ANil), "n" -> AInt(Some(0),Some(5))))))
  }

  //5.4. AStmt
  test("AAssign"){
    //initial state
    val as0 = AState(Map("xs" -> ACons(AInt(1), AMany(AInt.top)),
      "n" -> AInt(1), "i" -> AInt(2)))

    //assigning the constant value ANil to the handed over states with identifier xs
    var as1 = AAssign("xs", AConst(ANil)).execute(Set(as0))
    assert(as1 == Set(AState(Map("xs"-> ANil, "n" -> AInt(1), "i" -> AInt(2)))))

    //assigning the value of variable i to the handed over states with identifier n
    as1 = AAssign("n", AVar("i")).execute(as1)
    assert(as1 == Set(AState(Map("xs"-> ANil, "n" -> AInt(2), "i" -> AInt(2)))))

    //assigning the value of an expression to the handed over states with identifier i
    as1 = AAssign("i", AOp("+", List(AVar("i"), AVar("n")))).execute(as1)
    assert(as1 == Set(AState(Map("xs"-> ANil, "n" -> AInt(2), "i" -> AInt(4)))))

    /*assigning the constant value ATrue to the handed over states with identifier abc
     *would throw an exception since this program variable is not in the inital state */
    //as0 = AAssign("abc", AConst(ATrue)).execute(as0)

    assert(as1.head.lookup("xs") == ANil)
    assert(as1.head.lookup("n")  == AInt(2))
    assert(as1.head.lookup("i")  == AInt(4))
  }

  test("AAssume"){
    //initial state
    val as0 = AState(Map("xs" -> AMany(AInt(1)),"ys" -> AMany(AInt(2))))

    //Assumption: shape AMany is supposed to only execute the ACons-path
    val as1 = AAssume(! APred("isNil", "xs")).execute(Set(as0))
    val as2 = AAssume(! APred("isNil", "ys")).execute(as1)

    assert(as2.head.lookup("xs") == ACons(AInt(1), AMany(AInt(1))))
    assert(as2.head.lookup("ys") == ACons(AInt(2), AMany(AInt(2))))

    //asserting that there is only one program state after these assumptions
    assert(as2 == Set(AState(Map("xs" -> ACons(AInt(1), AMany(AInt(1))),
      "ys" -> ACons(AInt(2), AMany(AInt(2)))))))

    //Concatenating the two list values of shape ACons
    val as3 = AOp("concat", List(AVar("xs"), AVar("ys"))).evaluate(as2.head)

    assert(as3 == ACons(AInt(1), AMany(AInt(Some(1), Some(2)))))
  }

  test("AAssert"){
    //initial state
    var as0 = AState(Map("xs" -> ANil, "i"  -> AInt(2),
      "ys" -> AMany(AInt(Some(0), None)), "n"  -> AInt(-10)))

    //AAssert to be tested: xs is an empty list
    var assrt = AAssert(APred("isNil", "xs")).execute(Set(as0))
    //instead of assert(as0.head.lookup("xs" == ANil))

    //AAssert to be tested: i is a positive value
    assrt = AAssert(APred("isPositive", "i")).execute(assrt)

    //AAssert to be tested: n is not a positive value
    assrt = AAssert(!APred("isPositive", "n")).execute(assrt)

    //AAssert to be tested: ys is not an empty list
    assrt = AAssert(!APred("isNil", "ys")).execute(assrt)
    //This assertion will fail since AMany retruns a non-empty negative test set
  }

  test("AIf"){
    //initial state
    val as0 = AState(Map("n" -> AInt(-2), "xs" -> ANil))

    //test condition
    val test = APred("isPositive","n")

    //Abstract representation of an if-else-statement
    val as1 = AIf(test, AAssign("n", AConst(AInt(-11))), AAssign("n", AConst(AInt(9)))).execute(Set(as0))
    assert(as1 == Set(AState(Map("n" -> AInt(9), "xs" -> ANil))))

    //Abstract representation of an if-statement
    val as2 = AIf(test, AAssign("xs", AConst(AMany(AInt(0))))).execute(as1)
    assert(as2 == Set(AState(Map("n" -> AInt(9), "xs" -> AMany(AInt(0))))))
  }

  test("AWhile"){
    //initial state
    val as0 = AState(Map("n" -> AInt(2),
      "xs" -> ACons(AInt(1), ACons(AInt(2), AMany(AInt(3))))))

    //test condition
    val test = APred("isNil","xs")

    //Statements to be executed
    val body = ABlock(
      //assign xs.tailOption as new value of xs
      AAssign("xs", AOp("tail", List(AVar("xs")))),
      //assign xs.get as new value of xs
      AAssign("xs", AOp("get", List(AVar("xs")))),
      //subtract [1,1] of the current value in variable n
      AAssign("n", AOp("-", List(AVar("n"), AConst(AInt.one)))))

    //Execution of the abstracted while loop for the abstract state as0
    val prog = AWhile(!test, body, 5).execute(Set(as0))
    assert(prog == Set(AState(Map("n" -> AInt(None, Some(2)), "xs" -> ANil))))
  }





}
