import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class LoopTests extends AnyFunSuite {
  /** *******************************************************
   * tests: Loop Abstract                                  *
   * Tests are just using the abstract type AList          *
   * ****************************************************** */

  test("Concrete Example") {
    /**
     * int n
     * List xs
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

    var xs: b.AList = b.ACons(c, b.ACons(c, b.ANil))
    var n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))
    val i = b.intervals.Interval(IntegerVal(1), IntegerVal(1))

    //initial state
    var state = b.AState(n, xs)
   // val state2 = b.AState(i, b.justAList(b.aTail(xs)))


    val stmt1 = b.AssignN0
    val stmt2 = b.AssignN_Test1
    val stmt3 = b.AssignN_Test2
    //1. Iteration

    val if1 = b.IfxsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val h = if1.execute(state)

    println(h)

    state = h.head

    //2.Iteration
    val if2 = b.IfxsIsNil(stmt3,stmt2)  //stmt1, stmt2
    val j = if2.execute(state)

    println(j)


  }


  test("Loop Abstract 1 (n = 0, ASome)") {
    /**
     * AInt n
     * AList xs
     * while != isNil(xs){
     * xs = xs.tail
     * n++
     * }
     * assert isNil == xs
     * assert [0;0] <= n
     */
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))

    var xs: b.AList = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil)))
    var n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))
    var counter = b.intervals.Interval(IntegerVal(0), IntegerVal(0))
    val x = b.intervals.Interval(IntegerVal(0), IntegerVal(0))


    //1. Iteration
    println("(1) before xs: " + xs)
    var ys: b.AOption[b.AList] = b.aTail(xs)
    xs = b.justAList(ys)
    println("(1) ys: " + ys)
    xs = b.justAList(ys)
    println("(1) after xs: " + xs)
    n = b.intervals.Lattice.widen(n, counter)
    println("(1) n: " + n)
    println("")

    //2. Iteration
    println("(2) before xs: " + xs)
    ys = b.aTail(xs)
    xs = b.justAList(ys)
    println("(2) ys: " + ys)
    xs = b.justAList(ys)
    println("(2) after xs: " + xs)
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    n = b.intervals.Lattice.widen(n, counter)
    println("(1) n: " + n)
    println("")

    //3. Iteration
    println("(3) before xs: " + xs)
    ys = b.aTail(xs)
    xs = b.justAList(ys)
    println("(3) ys: " + ys)
    xs = b.justAList(ys)
    println("(3) after xs: " + xs)
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    n = b.intervals.Lattice.widen(n, counter)
    println("(1) n: " + n)
    println("")

    println("after loop xs: " + xs)
    println("after loop n: " + n)
    println("after loop i: " + counter)

    assert(xs == b.ANil)
    println(b.isNil(xs))
    assert(b.intervals.Lattice.<=(x, n))
  }


  test("Loop Abstract 1 (n = 0, AMaybe)") {
    /**
     * AInt n
     * AList xs
     * while != isNil(xs){
     * xs = xs.tail
     * n++
     * }
     * assert isNil == xs
     * assert [0;0] <= n
     */
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1), IntegerVal(5))

    var xs: b.AList = b.AMany(c)
    var n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))
    var counter = b.intervals.Interval(IntegerVal(0), IntegerVal(0))
    val x = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    println("(1) before xs: " + xs)
    var ys: b.AOption[b.AList] = b.aTail(xs)
    xs = b.justAList(ys)
    println("(1) ys: " + ys)
    xs = b.justAList(ys) //TODO check AMaybe != ANone
    println("(1) after xs: " + xs)
    n = b.intervals.Lattice.widen(n, counter)
    println("(1) n: " + n)
    println("")

    println("(2) before xs: " + xs)
    ys = b.aTail(xs)
    xs = b.justAList(ys)
    println("(2) ys: " + ys)
    xs = b.justAList(ys) //TODO check AMaybe != ANone
    println("(2) after xs: " + xs)
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    n = b.intervals.Lattice.widen(n, counter)
    println("(1) n: " + n)
    println("")

    println("(3) before xs: " + xs)
    ys = b.aTail(xs)
    xs = b.justAList(ys)
    println("(3) ys: " + ys)
    xs = b.justAList(ys) //TODO check AMaybe != ANone
    println("(3) after xs: " + xs)
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    n = b.intervals.Lattice.widen(n, counter)
    println("(1) n: " + n)
    println("")

    println("after loop xs: " + xs)
    println("after loop n: " + n)
    println("after loop i: " + counter)

    assert(xs == b.ANil) //TODO: AMany([-1;5]) did not equal ANil -> Check whether AMany is ANil or ACons(e, AMany(e))
    assert(b.intervals.Lattice.<=(x, n))

  }


  test("Append value(Abstract)") {
    /**
     * AInt i
     * AList xs
     * while(*){
     * i++
     * xs = i::xs
     * }
     *
     * assert aHead(xs) == i
     */

    val a = Intervals.Unbounded
    val b = ALists(a)
    var xs: b.AList = b.ANil
    var counter = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    println("(1) before xs: " + xs)
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    var i_head = b.intervals.Lattice.widen(counter, counter)
    xs = b.ACons(i_head, xs)
    println("(1) after xs: " + xs)
    println("")

    println("(2) before xs: " + xs)
    var n = counter
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    i_head = b.intervals.Lattice.widen(n, counter)
    xs = b.ACons(i_head, xs)
    println("(2) after xs: " + xs)
    println("")

    println("(3) before xs: " + xs)
    n = counter
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    i_head = b.intervals.Lattice.widen(n, counter)
    xs = b.ACons(i_head, xs)
    println("(3) after xs: " + xs)
    println("")

    assert(b.aHead(xs) == b.ASome(i_head))
    println(b.aHead(xs))
    println(b.ASome(i_head))

  }

}
