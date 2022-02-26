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
      case ("isNil", AMany(elems)) => Set(ANil)
      case ("isATrue", ATrue) => Set(ATrue)
      case ("isATrue", AFalse) => Set()
      case ("isATrue", AUnknown) => Set(ATrue)


      //TODO add missing tests (isZero, isOne, isTop, isNegative, isPositive, isEqual, contains)

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
      //TODO add missing tests
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

//TODO add missing tests (isZero, isOne, isTop, isNegative, isPositive, isEqual, ifIsATrue, contains)


