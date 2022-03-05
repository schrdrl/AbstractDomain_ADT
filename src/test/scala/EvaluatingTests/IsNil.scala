package EvaluatingTests
import AList_CleanCode.{ACons, AInt, ANil}
import org.scalatest.funsuite.AnyFunSuite

class IsNil extends AnyFunSuite {

  test("isNil-Concrete"){
    var xs = List(1,2,3,4,5)

    while(!xs.isEmpty){
      xs = xs.tail
    }
    assert(xs.isEmpty)
  }

  test("isNil-Abstract"){
    //var axs = ACons(AInt.top, ACons(AInt.top, ACons(AInt.top, ACons(AInt.top, ACons(AInt.top, ANil)))))

  }

  test("isNil-ATest"){

  }


}
