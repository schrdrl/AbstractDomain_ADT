package EvaluatingOperators
import AList_CleanCode.{AAssign, ABlock, ACons, AIf, AInt, AList, AMany, ANil, AOp, APred, AState, AVar}
import org.scalatest.funsuite.AnyFunSuite

class tail extends AnyFunSuite {
  //1
  test("tail: concrete (built-in method)"){
    val xs : List[Int] = List(9,7,4)
    val n = xs.tail
    println(n)

    val ys : List[Int] = List()
    val m  = ys.tail //throw exception
    println(m)
  }


  //2
  test("tail: abstract (built-in method)"){
    val xs : AList = ACons(AInt(9), ACons(AInt(7), AMany(AInt(4))))
    val n = xs.tail
    println(n)

    val ys : AList = ANil
    val o = ys.tail
    println(o)

    val zs : AList = AMany(AInt.top)
    val p = zs.tail
    println(p)
  }


  //3
  test("tail: abstract(AOp)"){

    val test = APred("isSome", "n")

    val prog = ABlock(AAssign("n", AOp("tail", List(AVar("xs")))), AIf(test, AAssign("n", AOp("get", List(AVar("n"))))))

    val as0 = Set(AState(Map("xs"-> ACons(AInt(9), ACons(AInt(7), AMany(AInt(4)))), "n" ->AInt.zero)),
      AState(Map("xs"-> ANil, "n" ->AInt.zero)),
      AState(Map("xs"-> AMany(AInt.top), "n" ->AInt.zero)))
    val as1 = prog.execute(as0)
    //println(as1)
    for(a<-as1) println(a)
  }

}
