package Expressions

import AList.Intervals
import sun.jvm.hotspot.utilities.Interval


trait AExpr {
  def evaluate(as: AState) : Any
  def +(that: AExpr) = ABinOp(this, "+", that) //nested expression

}

case class AState(values: Map[String, Any]) {
  def lookup(name: String): Any = values(name)

  def updated(name: String, value: Any) = AState(values+(name -> value))

}

case class AAssign(name: String, expr: AExpr)  {
   def execute(as: Set[AState]): Set[AState] = {
    for(a <- as) yield {
      val value = expr.evaluate(a)
      a.updated(name, value)
    }
  }
}

case class AConst(value:Any) extends AExpr {
  override def evaluate(as: AState): Any = value
}


case class APlus(left:AExpr, right: AExpr) extends AExpr {
  //1+1 oder n+1 oder n+m oder 1+(2+3)
  override def evaluate(as: AState): Any = {
    val l = left.evaluate(as)
    val r = right.evaluate(as)
    (l,r) match {
      case (l: Int, r: Int) => l+r
    }

  }
}

case class ABinOp(left:AExpr, op: String, right: AExpr) extends AExpr {
  override def evaluate(as: AState): Any = {
    val l = left.evaluate(as)
    val r = right.evaluate(as)
    (l,op,r) match {
      case (l: Int,"+" ,r: Int) => l+r
    }

  }
}

//AUnOp
case class AUnOp(op: String, aexpr: AExpr) extends AExpr {
  override def evaluate(as: AState): Any = {
    val ae = aexpr.evaluate(as)
    (op,ae) match {
      case ("aHead" ,ae: Int) => ???
    }

  }
}



case class AVar(name:String) extends AExpr {
  override def evaluate(as: AState): Any = as.lookup(name)
}