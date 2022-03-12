package EvaluatingOperators
import AList_CleanCode.{ACons, AInt, ANil}
import org.scalatest.funsuite.AnyFunSuite

class flatten extends AnyFunSuite {

  //1.
  test("flatten: concrete(built-in )"){
    val xs = List(List(1,2), List(3,4))
    val as0 = xs.flatten
    println(as0)

  }


  //2.
  test("flatten: abstract(built-in)"){
    val xs = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),ANil))))

    val as0 = xs.flatten
    println(as0)
  }

  //3.


}
