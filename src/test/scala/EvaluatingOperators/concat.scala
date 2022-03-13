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
    val c = AMany(AInt(10))
    val d = AMany(AInt.zero)

    //ANil ++ ANil
    val e = ANil.concat(ANil)
    println(e) //ANil

    //ANil ++ AMany
    val f = ANil.concat(AMany(AInt.zero))
    println(f) //AMany([0;0])

    //AMany ++ ANil
    val g = AMany(AInt.zero).concat(ANil)
    println(g) //AMany([0;0])

    //ANil ++ ACons
    val h = ANil.concat(a)
    println(h) //a

    //ACons ++ ANil
    val i = a.concat(ANil)
    println(i) //a

    //ACons ++ ACons
    val j = a.concat(b)
    println(j) //a++b

    //AMany ++ ACons
    val k = c.concat(b)
    println(k) //AMany([0,10])

    //ACons ++ AMany
    val l = b.concat(c)
    println(l) // b ++ c

    //ACons ++ AMany
    val b2 = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),AMany(AInt.zero)))))
    val m = b2.concat(c)
    println(m) // b ++ c


    //AMany ++ AMany
    val n = c.concat(d)
    println(n) // AMany([0,10])

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
