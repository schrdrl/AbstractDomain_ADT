package a

object Test {
  def bla(): Unit ={
    var n : Int = 0
    var xs : List[Int] = ???

    while (!xs.isEmpty){
      xs = xs.tail
      n = n + 1
    }
   assert (n >= 0)
  }



  def main(args: Array[String]) {
    val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt.top)))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("xs", AOp("tail", List(AVar("xs")))),
      AAssign("n", AOp("-", List(AVar("n"), AConst(AInt.one)))) //+
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test) //APred("isPositive", "n")
    )

    val as1 = prog.execute(as0)
    for(s <- as1) {
      s.lookup("n")//.hasConcreteElement
    }

    println(as1)
  }
}
