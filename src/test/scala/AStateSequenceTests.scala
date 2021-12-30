import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class AStateSequenceTests extends AnyFunSuite {
  test("AssignN0"){

    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AMany(c)
    val e = b.AState(c,d)

    val f = b.AssignN0
    val g = f.execute(e)
    println(g)

  }

  //TODO
  test("Sequence"){

    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AMany(c)
    val e = b.AState(c,d) //AState

    val f = b.AssignN0  //AStmt
    val f1 = f.execute(e)
    val g = b.AssignN1  //AStmt
    val g1 = g.execute(e)


    val h = b.Sequence(g,f)
    val i = h.execute(e)
    println(i)

  }

  test("ifIsNil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AMany(c)
    val e = b.ANil
    val f = b.ACons(c, b.ANil)
    val g = b.ACons(c, b.AMany(c))

    val (h1,h2) = b.ifIsNil(d)
    println(h1)
    println(h2)

    val (i1,i2) = b.ifIsNil(e)
    println(i1)
    println(i2)

    val (j1,j2) = b.ifIsNil(f)
    println(j1)
    println(j2)

    val (k1,k2) = b.ifIsNil(g)
    println(k1)
    println(k2)
  }

  //TODO
  test("IfxsIsNil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))

    //with AMany
    val d = b.AMany(c)
    val e = b.AState(c,d) //AState

    val f = b.AssignN0 //AStmt
    val g = b.AssignN1 //AStmt

    val h = b.IfxsIsNil(f,g)
    val h1 = h.execute(e)
    println(h1)

    //with ACons
    val i = b.ACons(c, b.ANil)
    val j = b.AState(c,i) //AState
    val k = b.IfxsIsNil(f,g)
    val k1 = k.execute(j)
    println(k1)

    //with ANil
    val l = b.ANil
    val m = b.AState(c,l) //AState
    val n = b.IfxsIsNil(f,g)
    val n1 = n.execute(m)
    println(n1)
  }


}
