package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, AAssume, ABlock, ACons, AConst, AIf, AInt, AList, AMany, AMaybe, ANil, ANone, AOp, APred, ASome, AState, ATrue, AUnknown, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite


class headOption extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("head and headOption: built-in methods Scala"){
    //using head on non-empty list
    val vs : List[Int] = List(9,7,4)
    assert(vs.head == 9)

    //using head on empty list
    val ws : List[Int] = List()
    ws.head //throws exception

    //using headOption on non-empty list
    val xs : List[Int] = List(9,7,4)
    assert(xs.headOption == Some(9))

    //using headOption on empty list
    val ys : List[Int] =  List()
    assert(ys.headOption == None)
  }


  /**
   * 1b. Abstract value (AList) + built-in method (AList)
   * Intention: show workaround of AOption (head,tail), demonstrate shapes of AList
   */
  test("headOption: built-in method abstract domain"){
    //test with ANil -> workaround with AOption: ANone
    val xs : AList = ANil
    val o = xs.headOption
    assert(o == ANone)

    val h1 = ANil.hasConcreteElement(List())
    assert(h1)

    //test with ACons
    val ys : AList = ACons(AInt(9), ACons(AInt(7), AMany(AInt(4))))
    val n = ys.headOption
    assert(n == ASome(AInt(9)))

    val h2 = ys.hasConcreteElement(List(9,7,4))
    assert(h2)

    //test with AMany -> workaround with AOption: AMaybe
    val zs : AList = AMany(AInt.top)
    val p = zs.headOption
    assert(p == AMaybe(AInt.top))

    val h3 = zs.hasConcreteElement(List(9,7,4))
    val h4 = zs.hasConcreteElement(List())
    assert(h3 && h4)

    //further tests
    assert(ANil.headOption                             == ANone)
    assert(ACons(AInt(7), AMany(AInt(4))).headOption   == ASome(AInt(7)))
    assert(ACons(AInt(None, Some(5)), ANil).headOption == ASome(AInt(None, Some(5))))
    assert(AMany(AInt(Some(-100), Some(5))).headOption == AMaybe(AInt(Some(-100), Some(5))))
    assert(AMany(AInt(Some(1),None)).headOption        == AMaybe(AInt(Some(1), None)))
  }


  //1c. Abstract value (AList) + AOp
  test("headOption: integration of AList.headOption into AOp"){
    val a = AOp("head", List(AVar("xs"))).evaluate(AState(Map("xs"->  ACons(AInt(7), AMany(AInt(4))))))
    assert(a == ASome(AInt(7)))

    val a1 = AOp("get", List(AOp("head", List(AVar("xs"))))).evaluate(AState(Map("xs"->  ACons(AInt(7), AMany(AInt(4))))))
    assert(a1 == AInt(7))

    val b = AOp("head", List(AVar("xs"))).evaluate(AState(Map("xs"-> ANil)))
    assert(b == ANone)

    val c = AOp("head", List(AVar("xs"))).evaluate(AState(Map("xs"-> AMany(AInt.top))))
    assert(c == AMaybe(AInt.top))
  }

  //1d. Abstract value (Alist) + AStmt
  test("head(usage of AList.head inn AStmt)"){
    //initial state
    val init = AState(Map("xs" ->  AMany(AInt(7)), "n" -> AInt.top))

    //Assignment: headOption-element of xs is assigned to n
    val assgnmt = AAssign("n", AOp("head", List(AVar("xs")))).execute(Set(init))
    assert(assgnmt.head.lookup("n") == AMaybe(AInt(7)))

    //Assumption: We assume that n is of shape ASome
    //test condition
    val test = APred("isSome", "n")
    val assmptn = AAssume(test).execute(assgnmt)
    assert(assmptn.head.lookup("n") == ASome(AInt(7)))

    //if-Statement
    val cond_stmt = AIf(test, AAssign("n", AConst(AInt(7)))).execute(assmptn)
    assert(cond_stmt.head.lookup("n") == AInt(7))
  }

}
