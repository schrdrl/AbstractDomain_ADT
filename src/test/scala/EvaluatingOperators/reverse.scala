package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, ABlock, ACons, AInt, AMany, ANil, AOp, APred, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class reverse extends AnyFunSuite {

  //1a. Concrete values + built-in method (Scala)
  test("reverse (built-in method (Scala))"){
    //test on non-empty list
    val a: List[Int] = List(0,1,2,3)

    val b = a.reverse
    println(b)

    //test on empty lists
    val c: List[Int] = List()

    val d = c.reverse
    println(d)

  }


  /**
   * 1b. Concrete values + built-in method (Scala) -> illustrated
   *    To illustrate how the built-in reverse method in Scala works
   *    and how the built-in method in the abstract domain of AList
   *    covers them all
   */
  test("reverse (illustrated)") {
    var xs: List[Int] = List(0,1,2,3)
    var temp : List[Int] = List()
    println("init: " +xs )

    //reverse elements of xs by prepending them to temp
    while (!xs.isEmpty) {
      temp =  xs.head +: temp
      xs = xs.tail
    }
    assert(xs.isEmpty)
    assert(!temp.isEmpty)

    //reassigning the elements to xs by appending them
    while (!temp.isEmpty) {
      xs = xs :+ temp.head
      temp = temp.tail
    }
    assert(!xs.isEmpty)
    assert(temp.isEmpty)

    println("out: " +xs)
  }


  //TODO -recheck
  /**
   * 1c. Abstract value + built-in method (Scala) -> illustrated
   *      - To illustrate that the principle is the same as in example 1b
   *      - additional: widen, fixpoint iteration
   */
  test("reverse (abstract: illustrated)") {
    val xs = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3), ANil))))
    val temp = ANil

    val init = AState(Map("n" -> AInt.zero,"xs" -> xs, "temp" -> temp))
    val as0 = Set(init)


    //reverse elements of xs by prepending them to temp
    val test_xs = APred("isNil", "xs")
    val test_temp = APred("isNil", "temp")

    var body = ABlock(
      AAssign("n", AOp("head", List(AVar("xs")))),                       //head of ys
      AAssign("n", AOp("get", List(AVar("n")))),
      AAssign("temp", AOp("prepend", List(AVar("temp"), AVar("n")))),  //prepend the head element to xs
      AAssign("xs", AOp("tail", List(AVar("xs")))),                   //reassign the tail of ys to ys
      AAssign("xs", AOp("get", List(AVar("xs")))),
    )

    var prog = ABlock(
      AWhile(!test_xs, body, 5),
      AAssert(test_xs),   //assert xs is empty
    )

    val as1 = prog.execute(as0)
    for(a <- as1) println("temp: " +a)

    //reassigning the elements to xs by appending them
    body = ABlock(
      AAssign("n", AOp("head", List(AVar("temp")))),
      AAssign("n", AOp("get", List(AVar("n")))),
      AAssign("xs", AOp("prepend", List(AVar("xs"), AVar("n")))),
      AAssign("temp", AOp("tail", List(AVar("temp")))),
      AAssign("temp", AOp("get", List(AVar("temp")))),
    )

    prog = ABlock(
      AWhile(!test_temp, body, 5),
      AAssert(test_temp) //assert temp is  empty
    )

    val as2 = prog.execute(as1)
    for(a <- as2){
      assert(a.lookup("xs").hasConcreteElement(List(3, 2, 1, 0)))
      println("out: " +a)
    }

  }


  //1d. Abstract value (AList) + built-in method (AList)
  test("reverse (built-in method (abstract domain))") {
    val a = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3), AMany(AInt.top)))))
    val b = ACons(AInt(None, Some(0)),AMany(AInt.top))
    val c = AMany(AInt(Some(0), None))

    //test with ANil
    val d = ANil.reverse()
    val h1 = d.hasConcreteElement(List())
    assert(h1)

    //tests with ACons
    val e = a.reverse()
    val h2 = e.hasConcreteElement(List(0,1,2,3))
    assert(h2)

    val f = b.reverse()
    val h3 = f.hasConcreteElement(List(0,1,2,3))
    assert(h3)

    //test with AMany
    val g = c.reverse()
    println(g)
    val h4 = g.hasConcreteElement(List(0,1,2,3))
    assert(h4)

  }


  //1e. Abstract value (AList) + AOp
  test("reverse (integration into AOp)"){
    val a = AState(Map("xs" ->ANil))
    val b = AState(Map("xs" ->AMany(AInt(None, Some(0)))))
    val c = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil)))
    val d = AState(Map("xs" -> ACons(AInt(None, Some(0)),AMany(AInt.top))))

    val op = AAssign("xs", AOp("reverse", List(AVar("xs"))))

    val as0 = op.execute(Set(a,b,c,d))
    for(a<-as0) println("out:" +a)
  }
}
