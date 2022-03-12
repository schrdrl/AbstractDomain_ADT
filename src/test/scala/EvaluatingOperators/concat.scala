package EvaluatingOperators
import AList_CleanCode.{AAssign, ACons, AInt, AMany, ANil, AOp, AState, AVar}
import org.scalatest.funsuite.AnyFunSuite

class concat extends AnyFunSuite {

  //1.
  test("concat (concrete)"){
    val xs: List[Int] = List(-1, -2, -3, -4)
    val ys: List[Int] = List(0,1,2,3)

    val zs = xs.concat(ys)
    println(zs)


  }

  //2.
  test("concat (built-in method on abstract Domain AList)") {
    val a = ACons(AInt(-4), ACons(AInt(-3), ACons(AInt(-2), ACons(AInt(-1),ANil))))
    val b = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),ANil))))
    val c = ANil
    val d = AMany(AInt.zero)

    val e = a.concat(b)
    println(e)

    val f = d.concat(b)
    println(f)


  }

  //3.
  test("concat (Test: integration of AList.concat into AOp)"){
    val a = AState(Map("xs" ->AMany(AInt(None, Some(0))), "ys" -> ANil, "zs"-> ANil))
    val b = AState(Map("xs" ->AMany(AInt(None, Some(0))), "ys" -> ACons(AInt(None, Some(0)),ANil), "zs"-> ANil))
    val c = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ANil, "zs"-> ANil))
    val d = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ACons(AInt(None, Some(0)),AMany(AInt.top)), "zs"-> ANil))

    val op = AAssign("zs", AOp("concat", List(AVar("xs"), AVar("ys"))))

    val as0 = op.execute(Set(a,b,c,d))
    for(a<-as0) println(a)
  }


  //reverse


  //concat two lists
}
