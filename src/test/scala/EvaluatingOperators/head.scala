package EvaluatingOperators
import org.scalatest.funsuite.AnyFunSuite

class head extends AnyFunSuite {

//concrete

  test("head (concrete)"){
    var n = 0
    var xs : List[Int] = List(9,7,4)  //only positive numbers


    while (!xs.isEmpty && n >= 0){
      n = xs.head
      println(n)
      xs = xs.tail
    }
    assert (n >= 0)
    assert (xs.isEmpty)

  }


  test("head (absolute)"){

  }

}
