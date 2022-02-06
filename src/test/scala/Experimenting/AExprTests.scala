package Experimenting

import AList.{ALists, IntegerVal, Intervals}
import Expressions.{AAssign, ABinOp, AConst, APlus, AState}
import org.scalatest.funsuite.AnyFunSuite
class AExprTests extends AnyFunSuite {

  test("General functionality"){

    val a = AState(Map(("x", 3), ("y", 4))) //new AState type
    println(a)
    println(a.values)
    println(a.lookup("y"))
    println("")

    val b = AConst("test")  //AConst examples
    val c = AConst(5)
    val d = AConst(true)
    println(b)
    println(b.value)
    println(c)
    println(c.evaluate(a))
    println(d)
    println("")

    val e = AAssign("x", d) //AAssign
    println(e)
    println(e.execute(Set(a)))
    val e_executed = e.execute(Set(a))
    println("")

    println(a.values)
    println(a.lookup("x"))
    println(e_executed)
    println("")

    val f = APlus(c,c)    //APlus
    println(f)
    println(f.evaluate(a))
    println("")


    val g = ABinOp(c, "+", c) //ABinOp
    println(g)
    println(g.evaluate(a))

  }


  test("Different operators on an AState"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(2),IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(1),IntegerVal(1))

    //initial state
    val state1 = b.AState(Map(("AInt",c), ("AList", b.ACons(c, b.ANil)), ("ABool", b.ATrue)))
    println("Initial state: "+state1)

    //first operation -> not really on the AState values
    val op1 = b.ABinOp(b.AConst(c), "+", b.AConst(d)).evaluate(state1)

    val state2 = b.AAssign("AInt", b.AConst(op1)).execute(Set(state1))
    println("After +: "+state2)

    val op2 = b.ABinOp(b.AConst(b.ACons(c,b.AMany(c))), "union", b.AConst(b.ACons(d,b.AMany(d)))).evaluate(state2.head)

    val state3 = b.AAssign("AList", b.AConst(op2)).execute(state2)
    println("After union: "+state3)

  }
  test("AState with AUnOp and ABinOp"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(2),IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(1),IntegerVal(1))

    val state = b.AState(Map(("AInt", b.ABinOp(b.AConst(c), "+", b.AConst(d))),("AList",b.ABinOp(b.AConst(b.ANil), "union", b.AConst(b.ACons(c, b.AMany(d)))) ) ))

    //evaluate ABinOp vale of AState
    val op1 = state.lookup("AInt")
    println(op1)
    val op1_ev = op1.asInstanceOf[b.ABinOp].evaluate(state)
    println(op1_ev)

    val state1 = b.AState(Map(("AInt",c), ("AList", b.ACons(c, b.ANil)), ("ABool", b.ATrue)))

    //use values of AState as aexpr value
    val op2 = b.ABinOp(b.AConst(state1.lookup("AList")), "union", b.AConst(b.AMany(d))).evaluate(state1)
    println(op2)

    //test for AState
    val stateOp = b.AState(Map( ("ABinOp", "+"), ("operand",c), ("operator", d)))
    if(stateOp.values.exists(_._1 == "ABinOp")){
      val value = b.ABinOp(b.AConst(stateOp.lookup("operand")), stateOp.lookup("ABinOp").asInstanceOf[String], b.AConst(stateOp.lookup("operator")) )
      println(value)
      println(value.evaluate(stateOp))
    }

  }

  test("combine AState"){
    val a = Intervals.Unbounded
    val b = ALists(a)

    val c = b.intervals.Interval(IntegerVal(2),IntegerVal(3))
    val d = b.intervals.Interval(IntegerVal(1),IntegerVal(1))
    val e = b.intervals.Interval(IntegerVal(0),IntegerVal(0))

    val state1 = b.AState(Map( ("AInt1", c) ,("AInt2", d)))
    println(state1)
    val state2 =  b.AState(Map( ("AInt3", e) ))
    println(state2)

    println(state1.values.+("AInt3" -> e))
    println(state1)
    //println(state1.values + state2)
    //println(state1.values.+(state2.values))

  }

}
