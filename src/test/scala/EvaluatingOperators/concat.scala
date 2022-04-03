package EvaluatingOperators
import AList_CleanCode.{AAssert, AAssign, AAssume, ABlock, ACons, AConst, AIf, AInt, AMany, ANil, AOp, APred, ASkip, AState, AVar, AWhile}
import org.scalatest.funsuite.AnyFunSuite

class concat extends AnyFunSuite {
  //not a lot of hasConcreteElement tests -> already multiple ones in the other tests


  //1a. Concrete values + built-in method (Scala)
  test("concat (built-in method (Scala))"){
    //test on non-empty lists
    val a: List[Int] = List(-1, -2, -3, -4)
    val b: List[Int] = List(0,1,2,3)

    val c = a.concat(b)
    assert(c == List(-1, -2, -3, -4, 0, 1, 2, 3))

    //test on empty lists
    val d: List[Int] = List()
    val e: List[Int] = List()

    val f = d.concat(e)
    assert(f == List())

    //test on empty and non empty list
    val g: List[Int] = List()
    val h: List[Int] = List(1,2,3)

    val i = g.concat(h)
    val j = g.concat(h)
    assert(i == List(1, 2, 3))
    assert(j == List(1, 2, 3))
  }


  /**
   * 1b. Concrete values + built-in method (Scala) -> illustrated
   *    To illustrate how the built-in concat method in Scala works
   *    and how the built-in method in the abstract domain of AList
   *    covers them all
   */
  test("concat (illustrated)") {
    var xs: List[Int] = List(-4, -3, -2, -1)
    var ys: List[Int] = List(0,1,2,3)
    println("init: " +xs +", " +ys)

    while (!ys.isEmpty) {
      xs = xs :+ ys.head
      ys = ys.tail
    }
    assert(ys.isEmpty)
    assert(!xs.isEmpty)
    println("out: " +xs)
  }


  /**
   * 1c. Abstract value + built-in method (Scala) -> illustrated
   *      - To illustrate that the principle is the same as in example 1b
   *      - additional: widen, fixpoint iteration
   */
  test("concat (abstract: illustrated)") {
    val xs = ACons(AInt(-4), ACons(AInt(-3), ACons(AInt(-2), ACons(AInt(-1), ANil))))
    val ys = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3), AMany(AInt.top)))))

    val h1 = xs.hasConcreteElement(List(-4, -3, -2, -1))
    val h2 = ys.hasConcreteElement(List(0,1,2,3))
    assert(h1 && h2)


    val init = AState(Map("n" -> AInt.zero,"xs" -> xs, "ys" -> ys))
    val as0 = Set(init)

    val test = APred("isNil", "ys")

    val body = ABlock(
      AAssign("n", AOp("head", List(AVar("ys")))),                //head of ys
      AAssign("n", AOp("get", List(AVar("n")))),
      AAssign("xs", AOp("append", List(AVar("xs"), AVar("n")))),  //append the head element to xs
      AAssign("ys", AOp("tail", List(AVar("ys")))),               //reassign the tail of ys to ys
      AAssign("ys", AOp("get", List(AVar("ys")))),
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test), //assert ys is empty
      AAssert(!APred("isNil", "xs")) //assert xs is not empty
    )

    val as1 = prog.execute(as0)
    for(a <- as1) {
      assert(a.lookup("xs").hasConcreteElement(List(-4, -3, -2, -1, 0, 1, 2, 3))) // -> return value of test 1c
      println("out: "+a)
    }
  }

  //1d. Abstract value (AList) + built-in method (AList)
  test("concat (built-in method (abstract domain))") {
    val a = ACons(AInt(-4), ACons(AInt(-3), ACons(AInt(-2), ACons(AInt(-1),ANil))))
    val b = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),ANil))))
    val c = AMany(AInt(10))
    val d = AMany(AInt.zero)

    val a_b = ACons(AInt(-4), ACons(AInt(-3), ACons(AInt(-2), ACons(AInt(-1),ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),ANil))))))))
    val b_a = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),ACons(AInt(-4), ACons(AInt(-3), ACons(AInt(-2), ACons(AInt(-1),ANil))))))))

    val b_c = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),AMany(AInt(10))))))
    val c_b = AMany(AInt(Some(0), Some(10)))

    //ANil ++ ANil
    val e = ANil.concat(ANil)
    assert(e == ANil) //ANil

    //ANil ++ AMany
    val f = ANil.concat(AMany(AInt.zero))
    assert(f == AMany(AInt.zero)) //AMany([0;0])

    //AMany ++ ANil
    val g = AMany(AInt.zero).concat(ANil)
    assert(g == AMany(AInt.zero))  //AMany([0;0])

    //ANil ++ ACons
    val h = ANil.concat(a)
    assert(h == a) //a

    //ACons ++ ANil
    val i = a.concat(ANil)
    assert(i == a) //a

    //ACons ++ ACons
    val j = a.concat(b)
    assert(j == a_b) //a++b
    assert(j != b_a)

    //AMany ++ ACons
    val k = c.concat(b)
    assert(k == c_b)//AMany([0,10])

    //ACons ++ AMany
    val l = b.concat(c)
    assert(l == b_c) // b ++ c
    assert(l != c_b)

    //ACons ++ AMany
    val b2 = ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),AMany(AInt.zero)))))
    val m = b2.concat(c)
    assert(m == ACons(AInt(0), ACons(AInt(1), ACons(AInt(2), ACons(AInt(3),AMany(AInt(Some(0), Some(10)))))))) // b2 ++ c

    //AMany ++ AMany
    val n = c.concat(d)
    assert(n == c_b) // AMany([0,10])

  }

  //1e. Abstract value (AList) + AOp
  test("concat (integration into AOp)"){
    val a = AState(Map("xs" ->AMany(AInt(None, Some(0))), "ys" -> ANil, "zs"-> ANil))
    val b = AState(Map("xs" ->AMany(AInt(None, Some(0))), "ys" -> ACons(AInt(None, Some(0)),ANil), "zs"-> ANil))
    val c = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ANil, "zs"-> ANil))
    val d = AState(Map("xs" ->ACons(AInt(None, Some(0)),ANil), "ys" -> ACons(AInt(None, Some(0)),AMany(AInt.top)), "zs"-> ANil))
    val as0 = Set(a,b,c,d)
    for(a <- as0) println("input: " +a)

    val op = AAssign("zs", AOp("concat", List(AVar("xs"), AVar("ys"))))

    val as1 = op.execute(as0)
    for(a <- as1) println("out: " +a)
  }



  //1f. concatenate two lists with positive elements, check whether the output list also contains only positive elements
  test("concat: positive elements"){
    val init = AState(Map("n" -> AInt.zero,"xs" -> AMany(AInt(Some(2), Some(10))), "ys" -> AMany(AInt(Some(5), Some(1999)))))
    val as0 = Set(init)

    //Assumption: AMany is not ANil
    val as1 = AAssume(!APred("isNil", "xs")).execute(as0)
    val as2 = AAssume(!APred("isNil", "ys")).execute(as1)

    //Test whether all elements of the given lists (-> AMany) are positive
   var prog = ABlock(AAssign("n", AOp("head", List(AVar("xs")))),                       //test for xs
                     AIf(APred("isSome", "n"), AAssign("n", AOp("get", List(AVar("n")))), ASkip),
                     AAssert(APred("isPositive", "n")),

                     AAssign("n", AOp("head", List(AVar("ys")))),                       //test for ys
                     AIf(APred("isSome", "n"), AAssign("n", AOp("get", List(AVar("n"))))),
                     AAssert(APred("isPositive", "n"))
               )

    val as3 = prog.execute(as2)
    println(as3)


    //concatenating the lists
    val as4 = AAssign("xs", AOp("concat", List(AVar("xs"), AVar("ys")))).execute(as3)
    println(as4)

    //check whether the output list also contains only positive elements
    prog = ABlock(AAssign("n", AOp("head", List(AVar("xs")))),
                  AIf(APred("isSome", "n"), AAssign("n", AOp("get", List(AVar("n"))))),
                  AAssert(APred("isPositive", "n"))
            )
    val as5 = prog.execute(as4)
    println(as5)
  }

  //maybe: concat: reverse -> append, prepend


}
