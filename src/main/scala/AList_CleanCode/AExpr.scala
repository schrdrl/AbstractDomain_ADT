package AList_CleanCode

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

//Binary & Unary
case class AOp(op: String, args: List[AExpr]) extends AExpr {
  def evaluate(as: AState): AVal = {
    val aes = args map (_.evaluate(as))

    (op, aes) match {

      //AInt
      case ("-", List(a: AInt)) => a.unary_-
      case ("abs", List(a: AInt)) => a.abs
      case ("+", List(l: AInt, r: AInt)) => l + r
      case ("-", List(l: AInt, r: AInt)) => l - r
      case ("*", List(l: AInt, r: AInt)) => l * r
      case ("/", List(l: AInt, r: AInt)) => l / r

      //ABool
      case ("!=", List(l:ABool, r:ABool)) => l.!=(r)
      case ("==", List(l:ABool, r:ABool)) => l.==(r)
      case ("&&", List(l:ABool, r:ABool)) => l.&&(r)
      case ("||", List(l:ABool, r:ABool)) => l.||(r)
      case ("!", List(ab:ABool)) => ab.!

      //AOption
      case("get", List(ASome(e))) => e //ANone, AMaybe -> should throw exception

      //AList
      case ("head", List(l:AList)) => l.head
      case ("tail", List(l:AList)) => l.tail
      case ("length", List(l: AList)) => l.length
      case("intersect", List(l: AList,r: AList)) => l.intersect(r)
      case("concat", List(l: AList,r: AList)) => l.concat(r)
      case("append", List(l: AList,r: AInt)) => l.append(r)
      case("prepend", List(l: AList,r: AInt)) => l.prepend(r)
      case("reverse", List(l: AList)) => l.reverse()



    }
  }
}
