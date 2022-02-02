package Experimenting

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

}
