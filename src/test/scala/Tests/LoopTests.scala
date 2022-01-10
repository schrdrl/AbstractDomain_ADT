package Tests

import AList.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class LoopTests extends AnyFunSuite {
  /** *******************************************************
   * tests: Loop Abstract                                  *
   * Tests are just using the abstract type AList          *
   * ****************************************************** */

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
    var state = Set(b.AState(n, xs))
    println("Initial State: " +state)

    val stmt1 = b.Assign_SameValues
    val stmt2 = b.Subtract1_ATail

    //1. Iteration
    val if1 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val h = if1.execute(state)
    println("after ifElse: " +h)

    if(h.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(h.head.n, h.tail.head.n), b.widen_AList(h.head.xs, h.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = h
    }


    //2.Iteration
    val if2 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val j = if2.execute(state)
    println("after ifElse: " +j)

    if(j.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(j.head.n, j.tail.head.n), b.widen_AList(j.head.xs, j.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = j
    }

    //3.Iteration
    val if3 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val k= if3.execute(state)
    println("after ifElse: " +k)

    if(k.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(k.head.n, k.tail.head.n), b.widen_AList(k.head.xs, k.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = k
    }

    //Translation between ABool and Boolean
    val bool_values = b.isNil(state.head.xs)
    assert((b.concretization_ABool(bool_values)).head == true)
    if(bool_values.size > 1) assert( (b.concretization_ABool(bool_values)).tail.head == false)
    assert(b.intervals.<=(state.head.n, n))

  }

  test("Concrete Example - ANil") {
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

    val xs: b.AList = b.ANil
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    var state = Set(b.AState(n, xs))
    println("Initial State: " +state)

    val stmt1 = b.Assign_SameValues
    val stmt2 = b.Subtract1_ATail

    //1. Iteration
    val if1 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val h = if1.execute(state)
    println("after ifElse: " +h)

    if(h.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(h.head.n, h.tail.head.n), b.widen_AList(h.head.xs, h.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = h
    }

    //2.Iteration
    val if2 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val j = if2.execute(state)
    println("after ifElse: " +j)

    if(j.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(j.head.n, j.tail.head.n), b.widen_AList(j.head.xs, j.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = j
    }

    //3.Iteration
    val if3 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val k= if3.execute(state)
    println("after ifElse: " +k)

    if(k.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(k.head.n, k.tail.head.n), b.widen_AList(k.head.xs, k.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = k
    }

    //Translation between ABool and Boolean
    val bool_values = b.isNil(state.head.xs)
    assert((b.concretization_ABool(bool_values)).head == true)
    if(bool_values.size > 1) assert( (b.concretization_ABool(bool_values)).tail.head == false)
    assert(b.intervals.<=(state.head.n, n))

  }

  test("Concrete Example - AMany") {
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

    val xs: b.AList = b.AMany(c)
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    var state = Set(b.AState(n, xs))   //AState([0;0], AMany([-1;5]))
    println("Initial State: " +state)

    //Statements of if-Sequence
    val stmt1 = b.Assign_SameValues
    val stmt2 = b.Subtract1_ATail

    //1. Iteration
    val if1 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val h = if1.execute(state)  //returns a Set of successors
    println("after ifElse: " +h)

    if(h.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(h.head.n, h.tail.head.n), b.widen_AList(h.head.xs, h.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = h
    }

    //2.Iteration
    val if2 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val j = if2.execute(state)
    println("after ifElse: " +j)

    if(j.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(j.head.n, j.tail.head.n), b.widen_AList(j.head.xs, j.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = j
    }

    //3.Iteration
    val if3 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val k= if3.execute(state)
    println("after ifElse: " +k)

    if(k.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(k.head.n, k.tail.head.n), b.widen_AList(k.head.xs, k.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = k
    }
    //Translation between ABool and Boolean
    val bool_values = b.isNil(state.head.xs)
    assert((b.concretization_ABool(bool_values)).head == true)
    if(bool_values.size > 1) assert( (b.concretization_ABool(bool_values)).tail.head == false)
    assert(b.intervals.<=(state.head.n, n))


  }

  test("Concrete Example - ACons(_,AMany)") {
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

    val xs: b.AList = b.ACons(c, b.AMany(c))
    val n = b.intervals.Interval(IntegerVal(0), IntegerVal(0))

    //initial state
    var state = Set(b.AState(n, xs))
    println("Initial State: " +state)

    val stmt1 = b.Assign_SameValues
    val stmt2 = b.Subtract1_ATail

    //1. Iteration
    val if1 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val h = if1.execute(state)
    println("after ifElse: " +h)

    if(h.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(h.head.n, h.tail.head.n), b.widen_AList(h.head.xs, h.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = h
    }


    //2.Iteration
    val if2 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val j = if2.execute(state)
    println("after ifElse: " +j)

    if(j.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(j.head.n, j.tail.head.n), b.widen_AList(j.head.xs, j.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = j
    }

    //3.Iteration
    val if3 = b.IfElse_xsIsNil(stmt1,stmt2)  //stmt1, stmt2
    val k= if3.execute(state)
    println("after ifElse: " +k)

    if(k.size > 1){
      state = Set(b.AState(b.intervals.Lattice.widen(k.head.n, k.tail.head.n), b.widen_AList(k.head.xs, k.tail.head.xs)))
      println("after widening: " +state)
    }else{
      state = k
    }

    //Translation between ABool and Boolean
    val bool_values = b.isNil(state.head.xs)
    assert((b.concretization_ABool(bool_values)).head == true)
    if(bool_values.size > 1) assert( (b.concretization_ABool(bool_values)).tail.head == false)
    assert(b.intervals.<=(state.head.n, n))

  }

  /** *******************************************************
   *       Tests without AState, AStmt, Sequences           *
   * ****************************************************** */

  test("Append value (Without AState, AStmt, Sequences)") {
    /**
     * int i
     * list xs
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

    //1.Iteration
    println("(1) before xs: " + xs)
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    var i_head = b.intervals.Lattice.widen(counter, counter)
    xs = b.ACons(i_head, xs)
    println("(1) after xs: " + xs +"\n")

    //2.Iteration
    println("(2) before xs: " + xs)
    var n = counter
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    i_head = b.intervals.Lattice.widen(n, counter)
    xs = b.ACons(i_head, xs)
    println("(2) after xs: " + xs +"\n")

    //3.Iteration
    println("(3) before xs: " + xs)
    n = counter
    counter = b.intervals.+(counter, b.intervals.Interval(IntegerVal(1), IntegerVal(1)))
    i_head = b.intervals.Lattice.widen(n, counter)
    xs = b.ACons(i_head, xs)
    println("(3) after xs: " + xs +"\n")


    assert(b.aHead(xs) == b.ASome(i_head))
  }

}
