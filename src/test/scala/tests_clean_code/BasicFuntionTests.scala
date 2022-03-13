package tests_clean_code
import AList_CleanCode.{ACons, AInt, AMany, AMaybe, ANil, ANone, ASome}
import org.scalatest.funsuite.AnyFunSuite
class BasicFuntionTests extends AnyFunSuite {



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
    println(AMany(c).intersect(e))
    println(ACons(c, ANil).intersect(ACons(a, ANil)))
    println(f.intersect(e))
    println(e.intersect(f))
    println(e.intersect(h))
    println(h.intersect(e))
    println(a.intersect(g))


  }


  test("reverse:AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = ACons(a, ACons(b, AMany(c)))
    val g = ACons(c, ACons(b, ANil))

    println(d.reverse())
    println(e.reverse())
    println(f.reverse())
    println(g.reverse())

  }

  test("union: AInt"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)
    val d = AInt.top
    val e = AInt(Some(0), None)
    val f = AInt(None, Some(0))

    println(a.union(b)) //[1;10]
    println(c.union(b)) //[-1;10]
    println(d.union(b)) //[-Inf;Inf]
    println(c.union(e)) //[-1;Inf]
    println(e.union(f)) //[-Inf;Inf]
    println(f.union(e)) //[-Inf;Inf]
  }

  test("intersect: AInt"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)
    val d = AInt.top
    val e = AInt(Some(0), None)
    val f = AInt(None, Some(0))

    println(a.intersect(b)) //ANone
    println(c.intersect(b)) //ANone
    println(d.intersect(b)) //TODO
    println(c.intersect(e)) //TODO
    println(e.intersect(f)) //TODO
    println(f.intersect(e)) //TODO
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

  //TODO
  test("===: AList"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANil
    val e = AMany(a)
    val f = AMany(c)
    val g = ACons(a, ACons(b, AMany(c)))
    val h = ACons(c, ACons(b, ANil))

    println("this: "+d)
    println("that: "+d)
    println(d.===(d)._1)
    println(d.===(d)._2 +"\n")

    println("this: "+d)
    println("that: "+e)
    println(d.===(e)._1)
    println(d.===(e)._2+"\n")

    println("this: "+e)
    println("that: "+d)
    println(e.===(d)._1)
    println(e.===(d)._2+"\n")

    println("this: "+e)
    println("that: "+f)
    println(e.===(f)._1)
    println(e.===(f)._2+"\n")

    println("this: "+f)
    println("that: "+e)
    println(f.===(e)._1)
    println(f.===(e)._2+"\n")

    //TODO ACons, ACons

    //TODO AMany, ACons

    //TODO ACons, AMany

  }


  test("===: AOption"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val d = ANone
    val e = ASome(a)
    val f = AMaybe(a)
    val g = ASome(b)
    val h = AMaybe(c)
    val i = ASome(c)

    println("this: "+d)
    println("that: "+d)
    println(d.===(d)+"\n")

    println("this: "+d)
    println("that: "+e)
    println(d.===(e)+"\n")

    println("this: "+d)
    println("that: "+f)
    println(d.===(f)+"\n")

    println("this: "+e)
    println("that: "+g)
    println(e.===(g)+"\n")

    println("this: "+h)
    println("that: "+e)
    println(h.===(e)+"\n")

    println("this: "+h)
    println("that: "+f)
    println(h.===(f)+"\n")

    println("this: "+i)
    println("that: "+e)
    println(i.===(e)+"\n")

    println("this: "+i)
    println("that: "+f)
    println(i.===(f)+"\n")


  }

  test("hasConcreteElement: AInt"){
    val a = AInt.one
    val b = AInt(Some(-1), Some(5))
    val c = AInt.top
    val d = AInt(None, Some(5))

    println(a.hasConcreteElement(1))
    println(a.hasConcreteElement(-1))

    println(b.hasConcreteElement(1))
    println(b.hasConcreteElement(-1))

    println(c.hasConcreteElement(1))
    println(c.hasConcreteElement(-1))

    println(d.hasConcreteElement(1))
    println(d.hasConcreteElement(-1))
    println(d.hasConcreteElement(6))


  }

  test("hasConcreteElement: AList"){
    val a = List()
    val b = List(1,2)
    val c = List(1)


    val d = ANil
    val e = AMany(AInt.one)
    val f = AMany(AInt.top)
    val g = ACons(AInt.one, ACons(AInt.top, ANil))

    println(d.hasConcreteElement(a))
    println(d.hasConcreteElement(b))
    println(d.hasConcreteElement(c) +"\n")

    println(e.hasConcreteElement(a))
    println(e.hasConcreteElement(b))
    println(e.hasConcreteElement(c)+"\n")

    println(f.hasConcreteElement(a))
    println(f.hasConcreteElement(b))
    println(f.hasConcreteElement(c)+"\n")

    println(g.hasConcreteElement(a))
    println(g.hasConcreteElement(b))
    println(g.hasConcreteElement(c)+"\n")
  }


}
