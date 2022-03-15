package EvaluatingOperators
import AList_CleanCode.{AAssign, ABlock, ACons, AIf, AInt, AList, AMany, ANil, ANone, AOp, APred, AState, AVar}
import org.scalatest.funsuite.AnyFunSuite

class tail extends AnyFunSuite {


  //1a. Concrete values + built-in method (Scala)
  test("tail (built-in method (Scala))"){
    //test on non-empty list
    val xs : List[Int] = List(9,7,4)
    val n = xs.tail
    println(n)

    //test on empty list
    val ys : List[Int] = List()
    val m  = ys.tail //throw exception: tail of empty list
  }


  /**
   * 1b. Abstract value (AList) + built-in method (AList)
   * Intention: show workaround of AOption (head,tail), demonstrate shapes of AList
   */
  test("tail (built-in method (abstract domain))"){

    //test with ANil -> workaround with AOption: ANone
    val ys : AList = ANil
    val o = ys.tail
    println(o)

    val h1 = ANil.hasConcreteElement(List())
    assert(h1)

    //test with ACons
    val xs : AList = ACons(AInt(9), ACons(AInt(7), AMany(AInt(4))))
    val n = xs.tail
    println(n)

    val h2 = xs.hasConcreteElement(List(9,7,4))
    assert(h2)

    //test with AMany -> workaround with AOption: AMaybe
    val zs : AList = AMany(AInt.top)
    val p = zs.tail
    println(p)

    val h3 = zs.hasConcreteElement(List(9,7,4))
    val h4 = zs.hasConcreteElement(List())
    assert(h3 && h4)

  }


  //1c. Abstract value (AList) + AOp
  test("tail (integration of AList.head into AOp)"){
    val test = APred("isSome", "temp")

    val prog = ABlock(AAssign("temp", AOp("tail", List(AVar("xs")))), AIf(test, AAssign("temp", AOp("get", List(AVar("temp"))))))

    val as0 = Set(AState(Map("xs"-> ACons(AInt(9), ACons(AInt(7), AMany(AInt(4)))), "temp" -> ANone)),
      AState(Map("xs"-> ANil, "temp" ->AInt.zero)),
      AState(Map("xs"-> AMany(AInt.top), "temp" ->AInt.zero)))

    for(a <- as0) println("init: " +a)

    val as1 = prog.execute(as0)

    for(a<-as1) println("out: "+a)
  }

}
