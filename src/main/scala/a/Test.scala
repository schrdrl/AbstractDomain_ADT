package a

object Test {
  def main(args: Array[String]) {
    val init = AState(Map("n" -> AInt.zero, "xs" -> AMany(AInt.top)))
    val as0 = Set(init)

    val test = APred("isNil", "xs")

    val body = ABlock(
      AAssign("xs", AOp("tail", List(AVar("xs")))),
      AAssign("n", AOp("-", List(AVar("n"), AConst(AInt.one))))
    )

    val prog = ABlock(
      AWhile(!test, body, 5),
      AAssert(test)
    )

    val as1 = prog.execute(as0)

    println(as1)
  }
}
