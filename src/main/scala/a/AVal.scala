package a

sealed trait AVal {
  def widen(that: AVal): AVal

  def widen(that: List[AVal]): AVal = {
    that match {
      case Nil           => this
      case first :: rest => (this widen first) widen rest
    }
  }
}

sealed trait ABool extends AVal {
  def widen(that: AVal): ABool = {
    (this, that) match {
      case _ if this == that    => ATrue
      case (_: ABool, _: ABool) => AUnknown
    }
  }
}

case object ATrue extends ABool
case object AFalse extends ABool
case object AUnknown extends ABool

object AInt {
  val top = AInt(None, None)
  val zero = AInt(0)
  val one = AInt(1)

  def apply(n: Int): AInt = AInt(Some(n), Some(n))
  def apply(l: Int, u: Int): AInt = AInt(Some(l), Some(u))

  def binop(op: (Int, Int) => Int, a: Option[Int], b: Option[Int]) = {
    (a, b) match {
      case (_, None)          => None
      case (None, _)          => None
      case (Some(a), Some(b)) => Some(op(a, b))
    }
  }

  def <(a: Option[Int], b: Option[Int]) = {
    (a, b) match {
      case (_, None)          => false
      case (None, Some(_))    => true
      case (Some(a), Some(b)) => (a < b)
    }
  }
}

case class AInt(lb: Option[Int], ub: Option[Int]) extends AVal {
  def unary_-(): AInt = {
    AInt(ub, lb)
  }

  def +(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val lb = AInt.binop(_ + _, this.lb, that.lb)
        val ub = AInt.binop(_ + _, this.ub, that.ub)
        AInt(lb, ub)
    }
  }

  def -(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val lb = AInt.binop(_ - _, this.lb, that.lb)
        val ub = AInt.binop(_ - _, this.ub, that.ub)
        AInt(lb, ub)
    }
  }

  def widen(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val lb = if (AInt.<(that.lb, this.lb)) None else this.lb
        val ub = if (AInt.<(this.ub, that.ub)) None else this.ub
        AInt(lb, ub)
    }
  }

  override def toString = {
    (lb, ub) match {
      case (None, None)         => "(-∞,∞)"
      case (Some(lb), None)     => "[" + lb + ",∞)"
      case (None, Some(ub))     => "(-∞," + ub + "]"
      case (Some(lb), Some(ub)) => "[" + lb + "," + ub + "]"
    }
  }
}

sealed trait AOption extends AVal {
  def widen(that: AVal): AOption = {
    (this, that) match {
      case (ANone, ANone) => ANone

      case (ANone, ASome(a)) => AMaybe(a)
      case (ASome(a), ANone) => AMaybe(a)

      case (ANone, AMaybe(a)) => AMaybe(a)
      case (AMaybe(a), ANone) => AMaybe(a)

      case (AMaybe(a), ASome(b)) => AMaybe(a widen b)
      case (ASome(a), AMaybe(b)) => AMaybe(a widen b)

      case (AMaybe(a), AMaybe(b)) => AMaybe(a widen b)
      case (ASome(a), ASome(b))   => ASome(a widen b)
    }
  }
}

case object ANone extends AOption
case class ASome(get: AVal) extends AOption
case class AMaybe(get: AVal) extends AOption

sealed trait AList extends AVal {
  def flatten: List[AVal]

  def widen(that: AVal): AList = {
    (this, that) match {
      case (ANil, ANil) => ANil

      case (ANil, ACons(a, as)) => AMany(a widen as.flatten)
      case (ACons(a, as), ANil) => AMany(a widen as.flatten)

      case (that: AList, AMany(e)) => AMany(e widen that.flatten)
      case (AMany(e), that: AList) => AMany(e widen that.flatten)

      case (ACons(a, as), ACons(b, bs)) => ACons(a widen b, as widen bs)
    }
  }
}

case object ANil extends AList {
  def flatten = Nil
}

case class ACons(head: AVal, tail: AList) extends AList {
  def flatten = head :: tail.flatten
}

case class AMany(elems: AVal) extends AList {
  def flatten = List(elems)
}
