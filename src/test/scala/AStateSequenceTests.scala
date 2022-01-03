import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class AStateSequenceTests extends AnyFunSuite {
  test("AssignN0") {

    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AMany(c)
    val e = b.AState(c, d)

    val f = b.AssignN0
    val g = f.execute(Set(e))
    println(g)
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
    println(h1,h2)

    val (i1,i2) = b.ifIsNil(e)
    println(i1,i2)

    val (j1,j2) = b.ifIsNil(f)
    println(j1,j2)

    val (k1,k2) = b.ifIsNil(g)
    println(k1,k2)
  }


  test("Add1 /Minus1"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))
    val d = b.AMany(c)
    val e = b.ACons(c, b.ANil)
    val f = b.AState(c, d)
    val g = b.AState(c, e)

    val h = b.AssignN_Add1
    val h1 = h.execute(Set(f))
    val h2 = h.execute(Set(f,g))

    println(h1)
    println(h2)

    val i = b.AssignN_Minus1
    val i1 = i.execute(Set(f))
    val i2 = i.execute(Set(f,g))

    println(i1)
    println(i2)

  }

  test("IfElse_xsIsNil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))

    //with AMany
    val d = b.AMany(c)
    val e = b.AState(c,d) //AState

    val f = b.AssignN0 //AStmt: AState([0;0], AList)
    val g = b.AssignN1 //AStmt

    val h = b.IfElse_xsIsNil(f,g)
    val h1 = h.execute(e)
    println(h1)

    //with ACons
    val i = b.ACons(c, b.ANil)
    val j = b.AState(c,i) //AState
    val k = b.IfElse_xsIsNil(f,g)
    val k1 = k.execute(j)
    println(k1)

    //with ANil
    val l = b.ANil
    val m = b.AState(c,l) //AState
    val n = b.IfElse_xsIsNil(f,g)
    val n1 = n.execute(m)
    println(n1)
  }

  test("If_xsIsNil"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))

    //with AMany
    val d = b.AMany(c)
    val e = b.AState(c,d) //AState

    val f = b.AssignN0 //AStmt: AState([0;0], AList)

    val h = b.If_xsIsNil(f)
    val h1 = h.execute(e)
    println(h1)

    //with ACons
    val i = b.ACons(c, b.ANil)
    val j = b.AState(c,i) //AState
    val k = b.If_xsIsNil(f)
    val k1 = k.execute(j)
    println(k1)

    //with ANil
    val l = b.ANil
    val m = b.AState(c,l) //AState
    val n = b.If_xsIsNil(f)
    val n1 = n.execute(m)
    println(n1)
  }


}