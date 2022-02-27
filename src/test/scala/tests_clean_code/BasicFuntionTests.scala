package tests_clean_code
import AList_CleanCode.{ACons, AInt, AMany, ANil}
import org.scalatest.funsuite.AnyFunSuite
class BasicFuntionTests extends AnyFunSuite {

  test("union:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))


    println(d.union(e))
    println(d.union(f))
    println(e.union(e))
    println(e.union(f))
  }

  test("intersect:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))

    println(d.intersect(e))
    println(d.intersect(f))
    println(e.intersect(e))
    println(e.intersect(f))
  }

  test("subset:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))

    println(d.subset(e))
    println(d.subset(f))
    println(f.subset(d))
    println(e.subset(e))
    println(e.subset(f))
  }

  test("reverse:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))
    val g = ACons(c, ACons(b, ANil))

    println(d.reverse())  //correct
    println(e.reverse())  //correct
    println(f.reverse()) //correct
    println(g.reverse()) //correct

  }


  test("===: AInt"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    println(a.===(b)._1)
    println(a.===(b)._2+"\n")

    println(a.===(c)._1)
    println(a.===(c)._2+"\n")

    println(b.===(a)._1)
    println(b.===(a)._2+"\n")

    println(b.===(c)._1)
    println(b.===(c)._2+"\n")

    println(c.===(a)._1)
    println(c.===(a)._2+"\n")

    println(c.===(b)._1)
    println(c.===(b)._2+"\n")
  }

}
