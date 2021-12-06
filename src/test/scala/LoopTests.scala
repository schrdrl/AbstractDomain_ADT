import Abstraction.{ALists, IntegerVal, Intervals}
import org.scalatest.funsuite.AnyFunSuite

class LoopTests extends AnyFunSuite {

  test("Loop Abstract 1 (n = 0, ASome)"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))

    var xs : b.AList = b.ACons(c, b.ACons(c, b.ACons(c, b.ANil)))
    var ys : b.AOption[b.AList] = b.ASome(xs)
    var b_AUnknown = true
    var n = 0

    println("befor loop: " +xs)
    while(b.isNil(xs) != b.ATrue && b_AUnknown == true){
      println("before xs: "+xs)
      ys = b.aTail(xs)
      println("ys: " +ys)
      if(b.isNil(xs) == b.AUnknown){
        b_AUnknown = false
      }
      xs = b.justAList(ys)
      println("after xs: "+xs)
      n += 1
    }
    println("after loop: "+xs)
    assert(n >= 0)
  }

  test("Loop Abstract 1 (n = 0, AMany)"){
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))

    var xs : b.AList = b.ACons(c, b.ACons(c, b.ACons(c, b.AMany(c))))
    var ys : b.AOption[b.AList] = b.ASome(xs)
    var b_AUnknown = true
    var n = 0

    println("befor loop: " +xs)
    while(b.isNil(xs) != b.ATrue && b_AUnknown == true){
      println("before xs: "+xs)
      ys = b.aTail(xs)
      println("ys: " +ys)
      if(b.isNil(xs) == b.AUnknown){
        b_AUnknown = false
      }
      xs = b.justAList(ys)
      println("after xs: "+xs)
      n += 1
    }
    println("after loop: "+xs)
    assert(n >= 0)

  }

  test("Loop Concrete 1"){
    var n = 0
    var xs : List[Int] = List(1,2,3)
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    val axs : b.AList = b.ACons(c,b.AMany(c))

    println("before loop: "+xs)
    while(xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue ){

      println("before xs: "+xs)
      xs = xs.tail
      n += 1
      println("after xs: "+xs)
    }
    println("after loop: "+xs)
    assert(n >= 0)
  }

  test("Loop Concrete 1 (ASome)"){
    var n = 0
    var xs : List[Int] = List(1,2,3)
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    var axs : b.AList = b.ACons(c,b.ACons(c, b.ACons(c, b.ANil)))
    var ys : b.AOption[b.AList] = b.ASome(axs)
    var b_AUnknown = true


    while(xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue && b_AUnknown == true){
      println("before xs: "+xs)
      println("before axs: "+axs)
      xs = xs.tail

      ys = b.aTail(axs)
      if(b.isNil(axs) == b.AUnknown){
        b_AUnknown = false
      }
      axs = b.justAList(ys)

      n += 1
      println("after xs: "+xs)
      println("after axs: "+axs)
    }
    println("after loop: "+xs)
    println("after loop: "+axs)
    assert(n >= 0)
  }

  test("Loop Concrete 1 (AMany)"){
    var n = 0
    var xs : List[Int] = List(1,2,3)
    val a = Intervals.Unbounded
    val b = ALists(a)
    val c = b.intervals.Interval(IntegerVal(-1),IntegerVal(5))
    var axs : b.AList = b.ACons(c,b.AMany(c))
    var ys : b.AOption[b.AList] = b.ASome(axs)
    var b_AUnknown = true


    while(xs != Nil && b.isConcreteElementOf_List(xs, axs) && b.isNil(axs) != b.ATrue && b_AUnknown == true){
      println("before xs: "+xs)
      println("before axs: "+axs)
      xs = xs.tail

      ys = b.aTail(axs)
      if(b.isNil(axs) == b.AUnknown){
        println("Test")
        b_AUnknown = false
      }
      axs = b.justAList(ys)

      n += 1
      println("after xs: "+xs)
      println("after axs: "+axs)
    }
    println("after loop: "+xs)
    println("after loop: "+axs)
    assert(n >= 0)
  }





}
