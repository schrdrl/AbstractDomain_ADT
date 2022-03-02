package AList_CleanCode

sealed trait AStmt {
  def execute(as: Set[AState]): Set[AState]
}

case object ASkip extends AStmt {
  def execute(as: Set[AState]): Set[AState] = {
    as
  }
}

case class AAssume(cond: ATest) extends AStmt {
  def execute(as: Set[AState]): Set[AState] = {
    cond.positive(as)
  }
}

case class AAssert(cond: ATest) extends AStmt {
  def execute(as: Set[AState]): Set[AState] = {
    val pos = cond.positive(as)
    val neg = cond.negative(as)

    if(neg.isEmpty) pos
    else {
      throw new Exception ("Exception occured while performing AAssert. Reason: neg is not empty")
    }
  }
}

//TODO AVerify

case class AAssign(name: String, expr: AExpr) extends AStmt {
  def execute(as: Set[AState]): Set[AState] = {
    for (a <- as) yield {
      val value = expr.evaluate(a)
      a.updated(name, value)
    }
  }
}

case class AIf(test: ATest, left: AStmt, right: AStmt = ASkip) extends AStmt {
  def execute(as: Set[AState]): Set[AState] = {
    val as1 = left.execute(test.positive(as))
    val as2 = right.execute(test.negative(as))
    as1 ++ as2
  }
}

object ABlock {
  def apply(stmts: AStmt*): ABlock = {
    ABlock(stmts.toList)
  }
}

case class ABlock(stmts: List[AStmt]) extends AStmt {
  def execute(as: Set[AState]): Set[AState] = {
    stmts.foldLeft(as) { case (as_, stmt) =>
      stmt.execute(as_)
    }
  }
}

//Abstract transformer: AWhile is an abstract representation of a while loop
case class AWhile(test: ATest, body: AStmt, max: Int = Int.MaxValue)
  extends AStmt {
  def execute(as0: Set[AState]): Set[AState] = {

    var as = AState.widenAll(as0)
    var bs = as
    var run = true

    var k = max

    while (run) {
      assert(k >= 0)
      k = k - 1

      val pos = test.positive(as)
      val cs = body.execute(pos)

      as = AState.widenAll(as ++ cs)
      run = (as != bs)
      bs = as
    }

    val neg = test.negative(as)
    neg
  }
}