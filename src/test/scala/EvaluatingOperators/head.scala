package EvaluatingOperators
import AList_CleanCode.{AAssign, ABlock, ACons, AIf, AInt, AList, AMany, AMaybe, ANil, ANone, AOp, APred, ASome, AState, ATrue, AUnknown, AVar}
import org.scalatest.funsuite.AnyFunSuite


class head extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("head (built-in method (Scala))"){
    //test on non-empty list
    val xs : List[Int] = List(9,7,4)
    val n = xs.head
    assert(n == 9)

    //test on empty list
    val ys : List[Int] = List()
    val m  = ys.head //throw exception: head of empty list

  }


  /**
   * 1b. Abstract value (AList) + built-in method (AList)
   * Intention: show workaround of AOption (head,tail), demonstrate shapes of AList
   */
  test("head (built-in method (abstract domain))"){

    //test with ANil -> workaround with AOption: ANone
    val xs : AList = ANil
    val o = xs.head
    assert(o == ANone)

    val h1 = ANil.hasConcreteElement(List())
    assert(h1)

    //test with ACons
    val ys : AList = ACons(AInt(9), ACons(AInt(7), AMany(AInt(4))))
    val n = ys.head
    assert(n == ASome(AInt(9)))

    val h2 = ys.hasConcreteElement(List(9,7,4))
    assert(h2)

    //test with AMany -> workaround with AOption: AMaybe
    val zs : AList = AMany(AInt.top)
    val p = zs.head
    assert(p == AMaybe(AInt.top))

    val h3 = zs.hasConcreteElement(List(9,7,4))
    val h4 = zs.hasConcreteElement(List())
    assert(h3 && h4)
  }


  //1c. Abstract value (AList) + AOp
  test("head (integration of AList.head into AOp)"){
    val test = APred("isSome", "n")

    val prog = ABlock(AAssign("n", AOp("head", List(AVar("xs")))), AIf(test, AAssign("n", AOp("get", List(AVar("n"))))))

    val as0 = Set(AState(Map("xs"-> ACons(AInt(9), ACons(AInt(7), AMany(AInt(4)))), "n" ->AInt.zero)),
                 AState(Map("xs"-> ANil, "n" ->AInt.zero)),
                 AState(Map("xs"-> AMany(AInt.top), "n" ->AInt.zero)))

    for(a <- as0) println("init: " +a)

    val as1 = prog.execute(as0)

    for(a <- as1) println("out: "+a)
  }
}
