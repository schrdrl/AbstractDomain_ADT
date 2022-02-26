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

      case ("+", List(l: AInt, r: AInt)) => l + r
      case ("-", List(l: AInt, r: AInt)) => l - r
      case ("*", List(l: AInt, r: AInt)) => l * r
      case ("/", List(l: AInt, r: AInt)) => l / r
      case("union", List(l: AInt,r: AInt)) => l.union(r)
      case("intersect", List(l: AInt,r: AInt)) => l.intersect(r)


      case ("!==", List(l:ABool, r:ABool)) => l.!==(r)
      case ("===", List(l:ABool, r:ABool)) => l.===(r)
      case ("&&", List(l:ABool, r:ABool)) => l.&&(r)
      case ("||", List(l:ABool, r:ABool)) => l.||(r)
      case ("!", List(ab:ABool)) => ab.!()

      //added AOption
      case ("aHead", List(ANil)) => ANone
      case ("aHead", List(ACons(head, _))) => ASome(head)
      case ("aHead", List(AMany(elems))) =>  AMaybe(elems)
      case ("aTail", List(ANil)) => ANone
      case ("aTail", List(ACons(_, tail))) => ASome(tail)
      case ("aTail", List(AMany(elems))) => AMaybe(elems)
      case ("aLength", List(ANil)) => ANone
      case ("aLength", List(ACons(head, tail))) => ASome(AInt(Some(0), Some(aes.head.asInstanceOf[AList].flatten.length)))
      case ("aLength", List(AMany(elems))) => AMaybe(AInt(Some(0),None))
      case("union", List(l: AList,r: AList)) => l.union(r)
      case("intersect", List(l: AList,r: AList)) => l.intersect(r)
      case("subset", List(l: AList,r: AList)) => l.subset(r)



      //TODO add missing methods
    }
  }
}
