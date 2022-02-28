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

    val g = AInt(None, Some(5))
    val h = AMany(g)

    println(d.intersect(e))
    println(d.intersect(f))
    println(e.intersect(e))
    println(e.intersect(f))
    println(e.intersect(h))
    println(h.intersect(e))
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
    val d = AInt.top
    val e = AInt(Some(0), None)
    val f = AInt(None, Some(0))

    println("this: "+a)
    println("that: "+b)
    println(a.===(b)._1)
    println(a.===(b)._2+"\n")

    println("this: "+a)
    println("that: "+c)
    println(a.===(c)._1)
    println(a.===(c)._2+"\n")

    println("this: "+b)
    println("that: "+a)
    println(b.===(a)._1)
    println(b.===(a)._2+"\n")

    println("this: "+b)
    println("that: "+c)
    println(b.===(c)._1)
    println(b.===(c)._2+"\n")

    println("this: "+c)
    println("that: "+a)
    println(c.===(a)._1)
    println(c.===(a)._2+"\n")

    println("this: "+c)
    println("that: "+b)
    println(c.===(b)._1)
    println(c.===(b)._2+"\n")

    println("this: "+c)
    println("that: "+d)
    println(c.===(d)._1)
    println(c.===(d)._2+"\n")

    println("this: "+c)
    println("that: "+e)
    println(c.===(e)._1)
    println(c.===(e)._2+"\n")

    println("this: "+c)
    println("that: "+f)
    println(c.===(f)._1)
    println(c.===(f)._2+"\n")

    println("this: "+d)
    println("that: "+e)
    println(d.===(e)._1)
    println(d.===(e)._2+"\n")

    println("this: "+e)
    println("that: "+d)
    println(e.===(d)._1)
    println(e.===(d)._2+"\n")

    println("this: "+d)
    println("that: "+f)
    println(d.===(f)._1)
    println(d.===(f)._2+"\n")

    println("this: "+f)
    println("that: "+d)
    println(f.===(d)._1)
    println(f.===(d)._2+"\n")


  }

}
