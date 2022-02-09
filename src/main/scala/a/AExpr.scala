package a

sealed trait AExpr {
  def evaluate(as: AState): AVal
  def +(that: AExpr) = AOp("+", List(this, that)) // nested expression
}

case class AConst(value: AVal) extends AExpr {
  def evaluate(as: AState): AVal = value
}

case class AVar(name: String) extends AExpr {
  def evaluate(as: AState): AVal = as.lookup(name)
}

case class AOp(op: String, args: List[AExpr]) extends AExpr {
  def evaluate(as: AState): AVal = {
    val aes = args map (_.evaluate(as))

    (op, aes) match {
      case ("-", List(a: AInt)) => -a

      case ("+", List(l: AInt, r: AInt)) => l + r
      case ("-", List(l: AInt, r: AInt)) => l - r

      case ("head", List(ACons(head, _))) => head
      case ("tail", List(ACons(_, tail))) => tail
    }
  }
}
