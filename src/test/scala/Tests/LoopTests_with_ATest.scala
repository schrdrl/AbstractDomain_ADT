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

    val xs: b.AList = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil))) //b.ACons(c, b.ACons(c, b.ANil))
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    val init_state = Set(b.AState(n, xs))
    println("Initial State: " + init_state)
    println("")

    //Test/Condition for while loop
    val conditionLoop = b.xsIsNotNilTest

    //Statement to execute in the while-loop
    val stmt = b.Subtract1_ATail

    //while loop
    val loopTest = b.AWhile(conditionLoop, stmt).execute(init_state)

    //Test/Condition for assertions
    val conditionAssert1 = b.xsIsNilTest
    val conditionAssert2 = b.nIsNegative

    //Assertions
    val assertAfterExecution1 = b.AAssert(conditionAssert1)
    println("")
    println("assert xs == nil")
    //TODO AIf(test, leftStmt, rightStmt)
    if (loopTest.nonEmpty) {
      assertAfterExecution1.execute(loopTest)
    } else {
      assertAfterExecution1.execute(init_state)
    }
    println("")
    println("assert n <= 0")
    val assertAfterExecution2 = b.AAssert(conditionAssert2)

    //TODO AIf(test, leftStmt, rightStmt)
    if (loopTest.nonEmpty) {
      assertAfterExecution2.execute(loopTest)
    } else {
      assertAfterExecution2.execute(init_state)
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

    val xs: b.AList = b.ANil
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    val init_state = Set(b.AState(n, xs))
    println("Initial State: " + init_state)
    println("")

    //Test/Condition for while loop
    val conditionLoop = b.xsIsNotNilTest

    //Statement to execute in the while-loop
    val stmt = b.Subtract1_ATail

    //while loop
    val loopTest = b.AWhile(conditionLoop, stmt).execute(init_state)

    //Test/Condition for assertions
    val conditionAssert1 = b.xsIsNilTest
    val conditionAssert2 = b.nIsNegative

    //Assertions
    val assertAfterExecution1 = b.AAssert(conditionAssert1)
    println("")
    println("assert xs == nil")

    //TODO AIf(test, leftStmt, rightStmt)
    if (loopTest.nonEmpty) {
      assertAfterExecution1.execute(loopTest)
    } else {
      assertAfterExecution1.execute(init_state)
    }
    println("")
    println("assert n <= 0")
    val assertAfterExecution2 = b.AAssert(conditionAssert2)

    //TODO AIf(test, leftStmt, rightStmt)
    if (loopTest.nonEmpty) {
      assertAfterExecution2.execute(loopTest)
    } else {
      assertAfterExecution2.execute(init_state)
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

    val xs: b.AList = b.AMany(c)
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    val init_state = Set(b.AState(n, xs))
    println("Initial State: " + init_state)
    println("")

    //Test/Condition for while loop
    val conditionLoop = b.xsIsNotNilTest

    //Statement to execute in the while-loop
    val stmt = b.Subtract1_ATail

    //while loop
    val loopTest = b.AWhile(conditionLoop, stmt).execute(init_state)

    //Test/Condition for assertions
    val conditionAssert1 = b.xsIsNilTest
    val conditionAssert2 = b.nIsNegative

    //Assertions
    val assertAfterExecution1 = b.AAssert(conditionAssert1)
    println("")
    println("assert xs == nil")
    //TODO AIf(test, leftStmt, rightStmt)
    if (loopTest.nonEmpty) {
      assertAfterExecution1.execute(loopTest)
    } else {
      assertAfterExecution1.execute(init_state)
    }
    println("")
    println("assert n <= 0")
    val assertAfterExecution2 = b.AAssert(conditionAssert2)

    //TODO AIf(test, leftStmt, rightStmt)
    if (loopTest.nonEmpty) {
      assertAfterExecution2.execute(loopTest)
    } else {
      assertAfterExecution2.execute(init_state)
    }
  }

//TODO output: code execution failed for:_ & successful for input:_
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
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    var counter = 0
    val axs: Set[b.AList] = Set(b.ANil, b.ACons(c, b.ANil), b.ACons(c, b.ACons(c, b.ANil)), b.ACons(c, b.AMany(c)), b.AMany(c))
    for (xs <- axs) {
      println("------ Round: " + counter +" ,initial state: " +xs +" -----")

      //initial state
      val init_state = Set(b.AState(n, xs))
      println("Initial State: " + init_state)
      println("")

      //Test/Condition for while loop
      val conditionLoop = b.xsIsNotNilTest

      //Statement to execute in the while-loop
      val stmt = b.Subtract1_ATail

      //while loop
      val loopTest = b.AWhile(conditionLoop, stmt).execute(init_state)

      //Test/Condition for assertions
      val conditionAssert1 = b.xsIsNilTest
      val conditionAssert2 = b.nIsNegative

      //Assertions
      val assertAfterExecution1 = b.AAssert(conditionAssert1)
      println("")
      println("assert xs == nil")
      //TODO AIf(test, leftStmt, rightStmt)
      if (loopTest.nonEmpty) {
        assertAfterExecution1.execute(loopTest)
      } else {
        assertAfterExecution1.execute(init_state)
      }
      println("")
      println("assert n <= 0")
      val assertAfterExecution2 = b.AAssert(conditionAssert2)

      //TODO AIf(test, leftStmt, rightStmt)
      if (loopTest.nonEmpty) {
        assertAfterExecution2.execute(loopTest)
      } else {
        assertAfterExecution2.execute(init_state)
      }
      println("------------------------------End Round: " + counter + "------------------------------")
      counter += 1
    }
  }

}
