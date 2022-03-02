package ExperimentalEvaluation
import AList_CleanCode.{AAssert, AAssign, ABlock, AConst, AInt, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class ConcatLists extends AnyFunSuite {
  /**
   * Two values of AList, both containing positive elements.
   * After concatenating these two values are all elements of the output value still positive?
   */

  test("concatenating two lists"){
    val axs = AMany(AInt(Some(0), Some(10)))
    val ays = AMany(AInt(Some(5), None)) //TODO AInt(Some(0), None)

    val init1 = AState(Map("xs" -> axs))
    val init2 = AState(Map("xs" -> ays))

    val as0 = Set(init1,init2)
    val as1 = List(AConst(axs), AConst(ays))

    val test = APred("isPositive", "xs")
    //check whether the lists contain only positive elements
    val arePos = AAssert(test).execute(as0)
    println("(is_pos,not_pos): "+arePos)

    //concatenating the lists
    val op = AOp("concat", as1).evaluate(init1) //: AList
    println("After concat: "+op)
    val init3 = AState(Map("xs" -> op))
    val as2 = Set(init3)

    //check whether the output also contains only positive elements
    val isPos = AAssert(test).execute(as2)
    println("(is_pos,not_pos): "+isPos)


  }
}
