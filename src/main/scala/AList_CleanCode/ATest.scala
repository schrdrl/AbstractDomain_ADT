package AList_CleanCode

sealed trait ATest {
  def unary_!() = ANot(this)

  def positive(as: AState): Set[AState]
  def negative(as: AState): Set[AState]

  def positive(as: Set[AState]): Set[AState] = {
    as flatMap positive
  }

  def negative(as: Set[AState]): Set[AState] = {
    as flatMap negative
  }
}

case class ANot(test: ATest) extends ATest {
  def positive(as: AState): Set[AState] = test.negative(as)
  def negative(as: AState): Set[AState] = test.positive(as)
}


case class APred(op: String, name: String) extends ATest {
  def positive(value: AVal): Set[AVal] = {
    (op, value) match {
      case ("isNil", ANil)         => Set(ANil)
      case ("isNil", ACons(_, _))  => Set()
      case ("isNil", AMany(_)) => Set(ANil)

      case ("isTrue", ATrue) => Set(ATrue)
      case ("isTrue", AFalse) => Set()
      case ("isTrue", AUnknown) => Set(ATrue)

      case ("isSome", ANone) => Set()
      case ("isSome", ASome(e)) => Set(ASome(e))
      case ("isSome", AMaybe(e)) => Set(ASome(e))

      case ("isNegative", ai : AInt) => if(ai.===(AInt(None, Some(0)))._1.nonEmpty) ai.===(AInt(None, Some(0)))._1 else Set()
      case ("isPositive", ai : AInt) => if(ai.===(AInt(Some(0), None))._1.nonEmpty) ai.===(AInt(Some(0), None))._1 else Set()

    }
  }

  def negative(value: AVal): Set[AVal] = {
    (op, value) match {
      case ("isNil", ANil)                 => Set()
      case ("isNil", ACons(_, _))          => Set(value)
      case ("isNil", value @ AMany(elems)) => Set(ACons(elems, value))

      case ("isTrue", ATrue) => Set()
      case ("isTrue", AFalse) => Set(AFalse)
      case ("isTrue", AUnknown) => Set(AFalse)

      case ("isSome", ANone) => Set(ANone)
      case ("isSome", ASome(e)) => Set()
      case ("isSome", AMaybe(e)) => Set(ANone)

      case ("isNegative", ai : AInt) => if(ai.===(AInt(None, Some(0)))._2.nonEmpty) ai.===(AInt(None, Some(0)))._2 else Set()
      case ("isPositive", ai : AInt) => if(ai.===(AInt(Some(0), None))._2.nonEmpty) ai.===(AInt(Some(0), None))._2 else Set()

    }
  }

  def positive(as: AState): Set[AState] = {
    for (newValue <- positive(as lookup name))
      yield as.updated(name, newValue)
  }

  def negative(as: AState): Set[AState] = {
    for (newValue <- negative(as lookup name))
      yield as.updated(name, newValue)
  }
}







