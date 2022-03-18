package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AInt, AMany, ANil, AOp, APred, AState, AVar}
import org.scalatest.funsuite.AnyFunSuite

class intersect extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("intersect (built-in method (Scala))"){
    val a = List(1,2,3,4)
    val b = List(2,3)
    val c = List(4,5)
    val d = List()

    //partially equal elements
    val as0 = a.intersect(b)
    println(as0)

    //partially equal elements
    val as1 = a.intersect(c)
    println(as1)

    //non-empty and empty list
    val as2 = a.intersect(d)
    println(as2)

    //non-empty lists with no equal elements
    val as3 = b.intersect(c)
    println(as3)

    //empty lists
    val as4 = d.intersect(d)
    println(as4)
  }


  //1b. Abstract value (AList) + built-in method (AList)
  test("intersect (built-in method (abstract domain))"){
    val a = ANil

    val b = ACons(AInt(None, Some(0)),ANil)
    val c = ACons(AInt(None, Some(0)),AMany(AInt.top))
    val d = ACons(AInt(Some(1), None),ANil)

    val e = AMany(AInt(None, Some(0)))
    val f = AMany(AInt(Some(-100), Some(0)))
    val g = AMany(AInt(Some(1), None))


    //ANil, ANil
    val as0 = a.intersect(a)
    println("this: "+a)
    println("that: "+a)
    println(as0+"\n")

    //ACons, ACons: partially equal elements
    val as1 = b.intersect(c)
    println("this: "+b)
    println("that: "+c)
    println(as1+"\n")

    //ACons, ACons: no equal parts
    val as2 = b.intersect(d)
    println("this: "+b)
    println("that: "+d)
    println(as2+"\n")

    //ACons, AMany: partially equal elements
    val as3 = b.intersect(e)
    println("this: "+b)
    println("that: "+e)
    println(as3+"\n")

    //ACons, AMany: no equal parts
    val as4 = b.intersect(g)
    println("this: "+b)
    println("that: "+g)
    println(as4+"\n")

    //AMany, AMany: partially equal elements
    val as5 = e.intersect(f)
    println("this: "+e)
    println("that: "+f)
    println(as5+"\n")

    //AMany, AMany: no equal parts
    val as6 = e.intersect(g)
    println("this: "+e)
    println("that: "+g)
    println(as6+"\n")

    //ANil and ACons
    val as7 = a.intersect(b)
    println("this: "+a)
    println("that: "+b)
    println(as7+"\n")

    //ANil and AMany
    val as8 = a.intersect(e)
    println("this: "+a)
    println("that: "+e)
    println(as8+"\n")

  }

  //1c. Abstract value (AList) + AOp
  test("intersect (integration of AList.intersect into AOp)"){
    // ANil, ANil
    val a = AState(Map("xs" ->ANil, "ys" -> ANil, "zs"-> ANil))
    //ACons, ACons: partially equal elements
    val b = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ACons(AInt(None, Some(0)),AMany(AInt.top)), "zs"-> ANil))
    //ACons, ACons: no equal parts
    val c = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ACons(AInt(Some(1), None),ANil), "zs"-> ANil))
    //ACons, AMany: partially equal elements
    val d = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> AMany(AInt(None, Some(0))), "zs"-> ANil))
    //ACons, AMany: no equal parts
    val e = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> AMany(AInt(Some(1), None)), "zs"-> ANil))
    //AMany, AMany: partially equal elements
    val f = AState(Map("xs" -> AMany(AInt(None, Some(0))), "ys" -> AMany(AInt(Some(-100), Some(0))), "zs"-> ANil))
    //AMany, AMany: no equal parts
    val g = AState(Map("xs" -> AMany(AInt(None, Some(0))), "ys" -> AMany(AInt(Some(1), None)), "zs"-> ANil))
    //ANil and ACons
    val h = AState(Map("xs" -> ANil , "ys" -> ACons(AInt(None, Some(0)),ANil), "zs"-> ANil))
    //ANil and AMany
    val i = AState(Map("xs" -> ANil, "ys" -> AMany(AInt(None, Some(0))), "zs"-> ANil))
    val as0 = Set(a,b,c,d,e,f,g,h,i)

    for(a <- as0) println("input: " +a)

    //assign output value to zs
    val op = AAssign("zs", AOp("intersect", List(AVar("xs"), AVar("ys"))))

    val as1 = op.execute(Set(a,b,c,d))
    for(a <- as1) println("out: " +a)

  }




}
