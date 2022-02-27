package tests_clean_code
import AList_CleanCode.{ACons, AInt, AMany, ANil, APred}
import org.scalatest.funsuite.AnyFunSuite
class ATestTests extends AnyFunSuite {

  test("isZero"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val pred1 = APred("isZero","n")
    println("pos: "+ pred1.positive(a))
    println("neg: "+pred1.negative(a))

    val pred2 = APred("isZero","n")
    println("pos: "+pred2.positive(b))
    println("neg: "+pred2.negative(b))

    val pred3 = APred("isZero","n")
    println("pos: "+pred3.positive(c))
    println("neg: "+pred3.negative(c))
  }

  test("isNegative"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val pred1 = APred("isNegative","n")
    println("pos: "+ pred1.positive(a))
    println("neg: "+pred1.negative(a))

    val pred2 = APred("isNegative","n")
    println("pos: "+pred2.positive(b))
    println("neg: "+pred2.negative(b))

    val pred3 = APred("isNegative","n")
    println("pos: "+pred3.positive(c))
    println("neg: "+pred3.negative(c))
  }

  test("isPositive"){
    val a = AInt.one
    val b = AInt.apply(10)
    val c = AInt.apply(-1, 5)

    val pred1 = APred("isPositive","n")
    println("pos: "+ pred1.positive(a))
    println("neg: "+pred1.negative(a))

    val pred2 = APred("isPositive","n")
    println("pos: "+pred2.positive(b))
    println("neg: "+pred2.negative(b))

    val pred3 = APred("isPositive","n")
    println("pos: "+pred3.positive(c))
    println("neg: "+pred3.negative(c))
  }


}
