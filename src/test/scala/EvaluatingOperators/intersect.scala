package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AInt, AMany, ANil, AOp, APred, AState, AVar}
import org.scalatest.funsuite.AnyFunSuite

class intersect extends AnyFunSuite {

  //1.built-in: Scala
  test("intersect: concrete(built-in method)"){
    val xs = List(1,2,3,4)
    val ys = List(2,3)
    val zs = List(4,5)

    val as0 = xs.intersect(ys)
    println(as0)

    val as1 = xs.intersect(zs)
    println(as1)

    val as2 = ys.intersect(zs)
    println(as2)
  }


  //2. built-in: Abstract Domain
  test("intersect: abstract(built-in method)"){
    val a = AMany(AInt(None, Some(0)))
    val b = ANil
    val c = ACons(AInt(None, Some(0)),ANil)
    val d = ACons(AInt(None, Some(0)),AMany(AInt.top))

    val as0 = a.intersect(b)
    println(as0)

    val as1 = a.intersect(c)
    println(as1)

    val as2 = c.intersect(b)
    println(as2)

    val as3 = c.intersect(d)
    println(as3)

  }

  //3. Integration into AOp
  test("intersect: abstract(integration into AOp"){
    val a = AState(Map("xs" ->AMany(AInt(None, Some(0))), "ys" -> ANil, "zs"-> ANil))
    val b = AState(Map("xs" ->AMany(AInt(None, Some(0))), "ys" -> ACons(AInt(None, Some(0)),ANil), "zs"-> ANil))
    val c = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ANil, "zs"-> ANil))
    val d = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ACons(AInt(None, Some(0)),AMany(AInt.top)), "zs"-> ANil))

    val op = AAssign("zs", AOp("intersect", List(AVar("xs"), AVar("ys"))))

    val as0 = op.execute(Set(a,b,c,d))
    println(as0)

  }




}
