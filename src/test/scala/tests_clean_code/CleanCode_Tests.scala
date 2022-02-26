package tests_clean_code

import AList_CleanCode.{ACons, AConst, AFalse, AInt, AMany, ANil, AOp, ASome, AState, ATrue, AUnknown, AVar}
import org.scalatest.funsuite.AnyFunSuite

class CleanCode_Tests extends AnyFunSuite {

  test("AVal"){

    //ABool
    val a = ATrue //case objects
    val b = AFalse
    val c = AUnknown

        //widen
        println("\nwiden: ABool")
        println(a.widen(a))
        println(b.widen(b))
        println(a.widen(b))
        println(a.widen(c))
        println(a.widen(List(b,c)))

    //AInt
    val d = AInt.one  //object
    val e = AInt.apply(10)
    val f = AInt.apply(-1, 5)
        //apply
        println("\napply: AInt")
        println(d)
        println(e)
        println(f)

        //binOp
    val g1 = AInt.binop(_+_, Some(1), Some(3))
    val g2 = AInt.binop(_+_, None, Some(3))
        println("\nbinOp: AInt")
        println(g1)
        println(g2)

        //<
        println("\n<: AInt")
        println(AInt.<(Some(1), Some(3)))

    val h = AInt(Some(1), Some(3)) //case class
        println(h)
        println(h.unary_-())
        println(h.+(e))

    //AList
    val i = ANil
    val j = AMany(h)
    val k = ACons(h, ACons(e, AMany(f)))
    println(i)
    println(j)
    println(k)

        //flatten
        println("\nflatten: AList")
        println(i.flatten)
        println(j.flatten)
        println(k.flatten)

        //widen
        println("\nwiden: AList")
        println(i.widen(j))
        println(i.widen(List(j)))
        println(i.widen(List(j,k)))
  }


  test("AState"){
    val a = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt.top)))    //case class
    val b = AState(Map())
    val c = AState(Map("n" -> AInt.zero, "xs" -> ACons(AInt.top, AMany(AInt.one))))
    val d = AState(Map("n" -> AInt.zero, "xs" -> ANil, "m" -> AInt.one, "ys" -> ANil))

    println(a)
    println(b)
    println(c)
    println(d)

    //lookup
    println("\nlookup")
    println(a.lookup("n"))
    println(a.vars)
    println(a.vars.tail)
    println(d.lookup("n"))
    println(d.lookup("m"))

    //updated
    println("\nupdated")
    val a2 = a.updated("xs", ANil)
    println(a2)

    //widen
    println("\nwiden")
    val a3 = a.widen(c)
    val a4 = a.widen(a2)

    println(a3)
    println(a4)

    //widenAll
    println("\nwidenAll")
    val state1 = AState.widenAll(Set(a,c))    //object
    println(state1)
    val state2 = AState.widenAll(Set())
    println(state2)

  }

  test("AExpr"){
    val a = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt.top), "b" -> AFalse))
    val b = AState(Map("n" -> AInt.zero, "xs" -> ANil))
    val c = AState(Map("n" -> AInt.zero, "xs" -> ACons(AInt.top, AMany(AInt.one))))

    val aconst1 = AConst(ATrue)
    val aconst2 = AConst(AInt(Some(-1), Some(5)))
    val aconst3 = AConst(ANil)
    val aconst4 = AConst(AMany(AInt(Some(-1), Some(5))))
    val aconst5 = AConst(ACons(AInt(Some(-1), Some(5)), ANil))

    println(aconst1.evaluate(a))
    println(aconst1)
    println(aconst2.evaluate(a))
    println(aconst3.evaluate(a))

    val avar1 = AVar("n")
    val avar2 = AVar("xs")
    val avar3 = AVar("b")

    println(avar1.evaluate(a))
    println(avar1)
    println(avar2.evaluate(a))
    println(avar3.evaluate(a))
    val avar_ev = avar1.evaluate(a)

    val aop1 = AOp("-", List(aconst2))
    println(aop1.evaluate(a))
    val aop2 = AOp("-", List(aconst2,aconst2))
    println(aop2.evaluate(a))

    val aop3 = AOp("aLength", List(aconst5))
    println("aLength "+aop3.evaluate(a))


  }





}
