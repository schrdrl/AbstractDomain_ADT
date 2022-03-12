package EvaluatingOperators

import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AConst, AIf, AInt, AList, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite


class unary extends AnyFunSuite {

  //integer value
  test ("Unary: Int") {
    var i: Int = - 1
    println (i)
    i = -i
    println (i)

    var j = 1
    println(j)
    j = -j
    println(j)
  }

  test("Unary (built-in method on abstract Domain AInt)") {
    val a = AInt(-5)
    val b = a.unary_-

    assert(AInt.<=(Some(0), b.lb) && AInt.<=(Some(0), b.ub))

    val c = AInt(5)
    val d = c.unary_-

    assert(AInt.<=(d.lb, Some(0)) && AInt.<=(d.ub, Some(0)))
  }


  test("Unary (Test: integration of AInt.unary into AOp)"){
    val as0 = Set(AState(Map("n"-> AInt(-5))), AState(Map("n"-> AInt(None,Some(-1))))) //negative values
    var test = APred("isPositive", "n")
    var prog = ABlock(AAssign("n", AOp("-", List(AVar("n")))), AAssert(test))

    val as2 = prog.execute(as0)
    println(as2)

    val as1 = Set(AState(Map("n"-> AInt(5))), AState(Map("n"-> AInt(Some(1), None)))) //positive values
    test = APred("isNegative", "n")
    prog = ABlock(AAssign("n", AOp("-", List(AVar("n")))), AAssert(test))

    val as3 = prog.execute(as1)
    println(as3)
  }


/*
  test("Unary:List (sorting lists)"){
    var xs : List[Int] = List(1,2,3,4,5,6)
    var ys : List[Int] = List()
    var zs : List[Int] = List()
    var n = xs.head

    //sorting values of xs after applying unary_- on certain values
    while(!xs.isEmpty){
      if(0 <= n){
        n = xs.head.unary_-
      }else{
        n = xs.head
      }

      if(n < 0) {
        ys = n :: ys
      }else{
        zs = n :: zs
      }
     xs = xs.tail
    }

    assert(xs.isEmpty)
    assert(!ys.isEmpty)
    assert(!zs.isEmpty)

    println("xs: "+xs)
    println("ys: "+ys)
    println("zs: "+zs +"\n")

    ys = ys.reverse
    zs = zs.reverse

    println("ys: "+ys)
    println("zs: "+zs)

  }


  //TODO: rethink test case structure
  test ("Unary: (Test: integration of AInt.unary into AWhile") {
    val xs = ACons(AInt(Some(0), None), ACons(AInt(Some(0), None), ACons(AInt(Some(0), None), ACons(AInt(Some(0), None), ACons(AInt(Some(0), None), ANil)))))
    val init = AState(Map("n" -> AInt.zero, "xs" -> xs, "ys" -> ANil, "zs" -> ANil))
    val as0 = Set(init)
    println("init: "+ as0)

    val test = APred("isNil", "xs")
    val test_elem = APred("isPositive", "n")
    val test_ys = APred("isNil", "ys")
    val test_zs = APred("isNil", "zs")


    //sorting values of xs after applying unary_- on certain values
    val body = ABlock(
      AIf(test_elem, ABlock(AAssign("n", AOp("head", List(AVar("xs")))), AAssign("n",AOp("just", List(AVar("n")))), AAssign("n", AOp("-", List(AVar("n"))))),ABlock(AAssign("n", AOp("head", List(AVar("xs")))), AAssign("n",AOp("just", List(AVar("n")))))),
      AIf(!test_elem, AAssign("ys", AOp("prepend", List(AVar("ys"), AVar("n")))), AAssign("zs", AOp("prepend", List(AVar("zs"), AVar("n"))))),
      AAssign("xs", AOp("tail", List(AVar("xs")))), AAssign("xs",AOp("just", List(AVar("xs"))))

    )

    val prog = ABlock(AWhile(!test, body, 5), AAssert(test))

    val as1 = prog.execute(as0)
    println(as1)


  }


 */

}

