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
      case ("-", List(a: AInt)) => -a
      case ("abs", List(a: AInt)) => a.abs

      case ("+", List(l: AInt, r: AInt)) => l + r
      case ("-", List(l: AInt, r: AInt)) => l - r
      case ("*", List(l: AInt, r: AInt)) => l * r
      case ("/", List(l: AInt, r: AInt)) => l / r
      case("union", List(l: AInt,r: AInt)) => l.union(r)
      case("intersect", List(l: AInt,r: AInt)) => l.intersect(r)

      case ("!=", List(l:ABool, r:ABool)) => l.noneq(r)
      case ("==", List(l:ABool, r:ABool)) => l.eq(r)
      case ("&&", List(l:ABool, r:ABool)) => l.&&(r)
      case ("||", List(l:ABool, r:ABool)) => l.||(r)
      case ("!", List(ab:ABool)) => ab.!

      case("just", List(ASome(e))) => e //ANone, AMaybe -> should throw exception

      case ("head", List(l:AList)) => l.head
      case ("tail", List(l:AList)) => l.tail
      case ("length", List(al: AList)) => al.length
      case("union", List(l: AList,r: AList)) => l.union(r)
      case("intersect", List(l: AList,r: AList)) => l.intersect(r)
      case("subset", List(l: AList,r: AList)) => l.subset(r)
      case("concat", List(l: AList,r: AList)) => l.concat(r)
      case("append", List(l: AList,r: AInt)) => l.append(r)
      case("prepend", List(l: AList,r: AInt)) => l.prepend(r)
      case("reverse", List(l: AList)) => l.reverse()



    }
  }
}
