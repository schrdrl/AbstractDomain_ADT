package EvaluatingOperators
import org.scalatest.funsuite.AnyFunSuite

class concat extends AnyFunSuite {

  test("concat (concrete)"){
    var xs: List[Int] = List(-1, -2, -3, -4)
    val ys: List[Int] = List(0,1,2,3,4)

    xs = xs ++ ys
    println(xs)


  }

  test("concat (built-in method on abstract Domain AList)") {
    ???
  }

  test("concat (Test: integration of AList.concat into AOp)"){

  }
}
