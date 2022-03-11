package EvaluatingOperators
import org.scalatest.funsuite.AnyFunSuite

class union extends AnyFunSuite {

  test("union: concrete-implemented method"){
    val xs = 1 until(4)
    val ys = 4 until(8)

    val zs = xs.union(ys)
    println(zs)


  }

}
