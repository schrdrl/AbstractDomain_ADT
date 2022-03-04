package ExperimentalEvaluation
import org.scalatest.funsuite.AnyFunSuite

class PrependElements extends AnyFunSuite {

  /**
   * val n : AInt = [0;0]
   * val xs : AList = ANil
   * while (*){
   *    n++
   *    xs = ACons(n, xs)
   * }
   *
   * assert xs.head == n
   * assert xs.tail ==[0; n.ub-1]
   * assert xs.length == [0; n] //0 <= n
   *
   */

}
