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
      case ("isATrue", ATrue) => Set(ATrue)
      case ("isATrue", AFalse) => Set()
      case ("isATrue", AUnknown) => Set(ATrue)
      case ("isZero", ai : AInt) => if(ai.split(AInt.zero.lb, "neither").nonEmpty) Set(AInt.zero) else Set()
      case ("isOne", ai : AInt) => if(ai.split(AInt.one.lb, "neither").nonEmpty) Set(AInt.one) else Set()
      case ("isNegative", ai : AInt) => if(ai.===(AInt(None, Some(0)))._1.nonEmpty) ai.===(AInt(None, Some(0)))._1 else Set()
      case ("isPositive", ai : AInt) => if(ai.===(AInt(Some(0), None))._1.nonEmpty) ai.===(AInt(Some(0), None))._1 else Set()
      case ("isASome", ANone) => Set()
      case ("isASome", ASome(e)) => Set(e)
      case ("isASome", AMaybe(e)) => Set(e)

    }
  }

  def negative(value: AVal): Set[AVal] = {
    (op, value) match {
      case ("isNil", ANil)                 => Set()
      case ("isNil", ACons(_, _))          => Set(value)
      case ("isNil", value @ AMany(elems)) => Set(ACons(elems, value))
      case ("isATrue", ATrue) => Set()
      case ("isATrue", AFalse) => Set(AFalse)
      case ("isATrue", AUnknown) => Set(AFalse)
      case ("isZero", ai : AInt) => if(ai.split(AInt.zero.lb, "neither").nonEmpty) ai.split(AInt.zero.lb, "neither").asInstanceOf[Set[AVal]] else Set(ai)
      case ("isOne", ai : AInt) => if(ai.split(AInt.one.lb, "neither").nonEmpty) ai.split(AInt.one.lb, "neither").asInstanceOf[Set[AVal]] else Set(ai)
      case ("isNegative", ai : AInt) => if(ai.===(AInt(None, Some(0)))._2.nonEmpty) ai.===(AInt(None, Some(0)))._2 else Set()
      case ("isPositive", ai : AInt) => if(ai.===(AInt(Some(0), None))._2.nonEmpty) ai.===(AInt(Some(0), None))._2 else Set()
      case ("isASome", ANone) => Set(ANone)
      case ("isASome", ASome(e)) => Set()
      case ("isASome", AMaybe(e)) => Set(ANone)
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







