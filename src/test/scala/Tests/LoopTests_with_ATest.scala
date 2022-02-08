package Tests

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class LoopTests_with_ATest extends AnyFunSuite {

  test("Concrete Example - ACons") {
    /**
     * int n
     * list xs
     * while(xs != Nil){
     * n = n-1
     * xs = xs.tail
     * }
     *
     * assert n <= 0
     * assert xs == nil
     */

    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(1), IntegerVal(1))

    val xs: b.AList = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil))) //b.ACons(c, b.ACons(c, b.ANil))
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    val init_state = Set(b.AState(Map(("AInt",n), ("AList", xs))))
    println("Initial State: " + init_state)
    println("")

    //Test/Condition for AWhile loop
    val testLoop = b.ATest(Set(b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))))

    //Statement to be executed in the AWhile-loop
    val stmt1=  b.AState(Map( ("ABinOp", "-"), ("operator", d), ("operand", "AInt")))  //subtract
    val stmt2= b.AState(Map(("AUnOp", "aTail"),("operand", "AList")))  //tail
    val stmt = b.AStmt(Set(stmt1, stmt2))

    //AWhile loop
    val awhile = b.AWhile(testLoop, stmt).execute(init_state)

    //AAssertions
    val assert1 =  b.AState(Map(("test", "ifIsNil"),("testedValue", "AList")))
    val assert2 =  b.AState(Map(("test", "ifIsNegative"),("testedValue", "AInt")))
    val assertions = b.ATest(Set(assert1, assert2))

    val aassert = b.AAssert(assertions).execute(awhile)
    //println(aassert)

    //AVerify
    if(aassert != (Set(), Set())){
      val averify = b.AVerify(assertions).execute(awhile)
    }else{
      val averify = b.AVerify(assertions).execute(init_state)
    }

  }


  test("Concrete Example - ANil") {
    /**
     * int n
     * list xs
     * while(xs != Nil){
     * n = n-1
     * xs = xs.tail
     * }
     *
     * assert n <= 0
     * assert xs == nil
     */

    val a = Intervals.Unbounded
    val b = ALists(a)
    val d = b.intervals.Interval(IntegerVal(1), IntegerVal(1))

    val xs: b.AList = b.ANil
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    val init_state = Set(b.AState(Map(("AInt",n), ("AList", xs))))
    println("Initial State: " + init_state)
    println("")

    //Test/Condition for AWhile loop
    val testLoop = b.ATest(Set(b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))))

    //Statement to be executed in the AWhile-loop
    val stmt1=  b.AState(Map( ("ABinOp", "-"), ("operator", d), ("operand", "AInt")))  //subtract
    val stmt2= b.AState(Map(("AUnOp", "aTail"),("operand", "AList")))  //tail
    val stmt = b.AStmt(Set(stmt1, stmt2))

    //AWhile loop
    val awhile = b.AWhile(testLoop, stmt).execute(init_state)

    //AAssertions
    val assert1 =  b.AState(Map(("test", "ifIsNil"),("testedValue", "AList")))
    val assert2 =  b.AState(Map(("test", "ifIsNegative"),("testedValue", "AInt")))
    val assertions = b.ATest(Set(assert1, assert2))

    val aassert = b.AAssert(assertions).execute(awhile)
    //println(aassert)

    //AVerify
    if(aassert != (Set(), Set())){
      val averify = b.AVerify(assertions).execute(awhile)
    }else{
      val averify = b.AVerify(assertions).execute(init_state)
    }
  }


  test("Concrete Example - AMany") {
    /**
     * int n
     * list xs
     * while(xs != Nil){
     * n = n-1
     * xs = xs.tail
     * }
     *
     * assert n <= 0
     * assert xs == nil
     */

    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(1), IntegerVal(1))

    val xs: b.AList = b.AMany(c)
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    val init_state = Set(b.AState(Map(("AInt",n), ("AList", xs))))
    println("Initial State: " + init_state)
    println("")

    //Test/Condition for AWhile loop
    val testLoop = b.ATest(Set(b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))))

    //Statement to be executed in the AWhile-loop
    val stmt1=  b.AState(Map( ("ABinOp", "-"), ("operator", d), ("operand", "AInt")))  //subtract
    val stmt2= b.AState(Map(("AUnOp", "aTail"),("operand", "AList")))  //tail
    val stmt = b.AStmt(Set(stmt1, stmt2))

    //AWhile loop
    val awhile = b.AWhile(testLoop, stmt).execute(init_state)

    //AAssertions
    val assert1 =  b.AState(Map(("test", "ifIsNil"),("testedValue", "AList")))
    val assert2 =  b.AState(Map(("test", "ifIsNegative"),("testedValue", "AInt")))
    val assertions = b.ATest(Set(assert1, assert2))

    val aassert = b.AAssert(assertions).execute(awhile)
    //println(aassert)

    //AVerify
    if(aassert != (Set(), Set())){
      val averify = b.AVerify(assertions).execute(awhile)
    }else{
      val averify = b.AVerify(assertions).execute(init_state)
    }
  }




  test("Concrete Example - Set[AList]") {
    /**
     * int n
     * list xs
     * while(xs != Nil){
     * n = n-1
     * xs = xs.tail
     * }
     *
     * assert n <= 0
     * assert xs == nil
     */
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.intervals.Interval(IntegerVal(1), IntegerVal(1))
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    var counter = 1
    val axs: Set[b.AList] = Set(b.ANil, b.ACons(c, b.ANil), b.ACons(c, b.ACons(c, b.ANil)), b.ACons(c, b.AMany(c)), b.AMany(c))
    for (xs <- axs) {
      println("--------------------------------- Round: " + counter + "------------------------------")

      //initial state
      val init_state = Set(b.AState(Map(("AInt",n), ("AList", xs))))
      println("Initial State: " + init_state)
      println("")

      //Test/Condition for AWhile loop
      val testLoop = b.ATest(Set(b.AState(Map(("test", "ifIsNotNil"),("testedValue", "AList")))))

      //Statement to be executed in the AWhile-loop
      val stmt1=  b.AState(Map( ("ABinOp", "-"), ("operator", d), ("operand", "AInt")))  //subtract
      val stmt2= b.AState(Map(("AUnOp", "aTail"),("operand", "AList")))  //tail
      val stmt = b.AStmt(Set(stmt1, stmt2))

      //AWhile loop
      val awhile = b.AWhile(testLoop, stmt).execute(init_state)

      //AAssertions
      val assert1 =  b.AState(Map(("test", "ifIsNil"),("testedValue", "AList")))
      val assert2 =  b.AState(Map(("test", "ifIsNegative"),("testedValue", "AInt")))
      val assertions = b.ATest(Set(assert1, assert2))

      val aassert = b.AAssert(assertions).execute(awhile)
      //println(aassert)

      //AVerify
      if(aassert != (Set(), Set())){
        val averify = b.AVerify(assertions).execute(awhile)
      }else{
        val averify = b.AVerify(assertions).execute(init_state)
      }
      println("------------------------------ End Round: " + counter + "-----------------------------\n")
      counter += 1
    }
  }

}
