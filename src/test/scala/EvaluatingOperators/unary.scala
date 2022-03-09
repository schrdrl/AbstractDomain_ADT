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
    var xs : List[Int] = List()
    var ys : List[Int] = List()
    var zs : List[Int] = List()
    var n = 6
    var i = 6

    //fill list with values
    while(i > 0){
      n = n.unary_-
      xs = n :: xs

      if(n < 0) n = n + 1
      else n = n-1

      i = i-1
    }
    println("xs: "+xs)

    //sorting values of xs
    while(!xs.isEmpty){
      if(xs.head > 0){
        ys = xs.head :: ys
      }else{
        zs = xs.head :: zs
      }
      xs = xs.tail
    }
    assert(xs.isEmpty)
    assert(!ys.isEmpty)
    assert(!zs.isEmpty)

    ys = ys.reverse
    zs = zs.reverse

    println("xs: "+xs)
    println("ys: "+ys)
    println("zs: "+zs)

  }


  test ("Unary: (Test: integration of AInt.unary into AWhile") {
    val init = AState(Map("i" -> AInt(6),"n" -> AInt(6), "xs" -> ANil)) //"ys" -> ANil, "zs" -> ANil
    val as0 = Set(init)
    println("init: "+ as0)

    val test = APred("isZero", "i")
    val test_elem = APred("isNegative", "n")


    //fill xs with values
    val body = ABlock(
      AAssign("n", AOp("abs", List(AVar("n")))),
      AAssign("n",AOp("-", List(AVar("n")))),                     //n.unary
      AAssign("xs", AOp("append", List(AVar("xs"), AVar("n")))),  //xs = n :: xs
                                                                  //AIf(n < 0) n = n+1 else n = n-1
      AIf(test_elem, AAssign("n", AOp("+", List(AVar("n"), AConst(AInt.one)))), AAssign("n", AOp("-", List(AVar("n"), AConst(AInt.one))))),
      AAssign("i", AOp("-", List(AVar("i"), AConst(AInt.one))))   //i = i - 1
    )

    val prog = ABlock(AWhile(!test, body, 5), AAssert(test))

    val as1 = prog.execute(as0)
    println(as1)

    //sort values of xs

  }
 */



}

