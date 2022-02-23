package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class NewAStmtTests extends AnyFunSuite {

  test("General functionality: AStmt"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(2),IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(1),IntegerVal(1))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))

    val f = b.ANil
    val g = b.AMany(c)
    val h = b.ACons(d, b.AMany(e))

    val op1 = b.AState(Map( ("ABinOp", "-"), ("operator", d), ("operand", "AInt"))) // n - [1;1]
    val op2 = b.AState(Map(("AUnOp", "aTail"),("operand", "AList")))

    val state1 = b.AState(Map( ("AList", f), ("AInt", c), ("ABool", b.ATrue)))
    val state2 = b.AState(Map( ("AList", g), ("AInt", d), ("ABool", b.ATrue)))
    val state3 = b.AState(Map( ("AList", h), ("AInt", e), ("ABool", b.ATrue)))

    val expressions = Set(op1, op2)

    val stmt = b.AStmt(expressions)
    println("AStmt: "+stmt + "\n")

    println("AStates: " +Set(state1, state2, state3) + "\n")

    val stmt_exe = stmt.execute(Set(state1, state2, state3))
    println("After execution: "+stmt_exe + "\n")



  }

  test("getOperand"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(2),IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(1),IntegerVal(1))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))

    val f = b.ANil
    val g = b.AMany(c)
    val h = b.ACons(d, b.AMany(e))

    val op1 = b.AState(Map( ("ABinOp", "-"), ("operator", d), ("operand", "AInt")))
    val op2 = b.AState(Map(("AUnOp", "aTail"),("operand", "AList")))

    val getOp = b.getOperands(Set(op1, op2))

    println(getOp)
  }

}
