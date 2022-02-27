package AList_CleanCode

sealed trait AVal {

  def widen(that: AVal): AVal

  def widen(that: List[AVal]): AVal = {
    that match {
      case Nil => this
      case first :: rest => (this widen first) widen rest
    }
  }

  def justValue(): AVal = {
    this match {
      case ASome(a) => a
      case AMaybe(_) => throw new Exception("Exception thrown from justValue. Reason: Input was AMaybe")
      case ANone => throw new Exception("Exception thrown from justValue. Reason: Input was ANone")
    }
  }
}

sealed trait ABool extends AVal {
  def widen(that: AVal): ABool = {
    (this, that) match {
      case (ATrue, ATrue) => ATrue //updated: instead of ==
      case (AFalse, AFalse) => AFalse //updated: instead of ==
      case (_: ABool, _: ABool) => AUnknown
    }
  }

  def ===(that: ABool): ABool = {
    (this, that) match {
      case (AFalse, AFalse) | (ATrue, ATrue) => ATrue
      case (AFalse, ATrue) | (ATrue, AFalse) => AFalse
      case (_: ABool, _: ABool) => AUnknown
    }
  }

  def !==(that: ABool): ABool = {
    (this, that) match {
      case (AFalse, AFalse) | (ATrue, ATrue) => AFalse
      case (AFalse, ATrue) | (ATrue, AFalse) => ATrue
      case (_: ABool, _: ABool) => AUnknown
    }
  }

  def &&(that: AVal): ABool = {
    (this, that) match {
      case (ATrue, ATrue) => ATrue
      case (AFalse, ATrue) | (ATrue, AFalse) | (AFalse, AFalse) => AFalse
      case (_: ABool, _: ABool) => AUnknown
    }
  }

  def ||(that: AVal): ABool = {
    (this, that) match {
      case (ATrue, _) | (_, ATrue) => ATrue
      case (AFalse, AFalse) => AFalse
      case (_: ABool, _: ABool) => AUnknown
    }
  }

  def !(): ABool = {
    this match {
      case AFalse => ATrue
      case ATrue => AFalse
      case AUnknown => AUnknown
    }
  }
}

case object ATrue extends ABool

case object AFalse extends ABool

case object AUnknown extends ABool

object AInt {
  val top: AInt = AInt(None, None)
  val zero: AInt = AInt(0)
  val one: AInt = AInt(1)

  def apply(n: Int): AInt = AInt(Some(n), Some(n))

  def apply(l: Int, u: Int): AInt = AInt(Some(l), Some(u))

  def binop(op: (Int, Int) => Int, a: Option[Int], b: Option[Int]): Option[Int] = {
    (a, b) match {
      case (_, None) => None
      case (None, _) => None
      case (Some(a), Some(b)) => Some(op(a, b))
    }
  }

  def <(a: Option[Int], b: Option[Int]): Boolean = {
    (a, b) match {
      case (_, None) => false
      case (None, Some(_)) => true
      case (Some(a), Some(b)) => a < b
    }
  }

  def <=(a: Option[Int], b: Option[Int]): Boolean = {
    (a, b) match {
      case (_, None) => false
      case (None, Some(_)) => true
      case (Some(a), Some(b)) => a < b || a == b
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


  def *(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val lb = AInt.binop(_ * _, this.lb, that.lb)
        val ub = AInt.binop(_ * _, this.ub, that.ub)
        AInt(lb, ub)
    }
  }

  def /(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val lb = AInt.binop(_ / _, this.lb, that.lb)
        val ub = AInt.binop(_ / _, this.ub, that.ub)
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

  override def toString: String = {
    (lb, ub) match {
      case (None, None) => "(-∞,∞)"
      case (Some(lb), None) => "[" + lb + ",∞)"
      case (None, Some(ub)) => "(-∞," + ub + "]"
      case (Some(lb), Some(ub)) => "[" + lb + "," + ub + "]"
    }
  }

  //TODO recheck -> maybe move to ATest if useful
  def contains(that: AVal): ABool = {
    that match {
      case that: AInt =>
        if (AInt.<=(this.lb, that.lb) && AInt.<=(that.ub, this.ub)) ATrue
        else AFalse
    }
  }

  def union(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val newlb = if (AInt.<(this.lb, that.lb)) this.lb else that.lb
        val newub = if (AInt.<(this.ub, that.ub)) that.ub else this.ub
        AInt(newlb, newub)
    }
  }

  //TODO what happens if one value is None
  def intersect(that: AVal): AOption = {
    that match {
      case that: AInt =>
        if (this == that) ASome(this)
        else {
          val newlb = if (AInt.<=(this.lb, that.lb)) that.lb else if (AInt.<=(that.lb, this.lb)) this.lb else ANone
          val newub = if (AInt.<=(this.ub, that.ub)) this.ub else if (AInt.<=(that.ub, this.ub)) that.ub else ANone
          if (newlb == ANone && newub == ANone) {
            ANone
          } else {
            ASome(AInt.apply(newlb.asInstanceOf[Option[Int]], newub.asInstanceOf[Option[Int]]))
          }
        }
    }
  }

  //TODO what happens if one value is None
  def split(that: Option[Int], s: String): Set[AInt] = {
    that match {
      case that: Option[Int] =>
        if (AInt.<(that, this.lb) || AInt.<(this.ub, that)) Set() //that is not in this
        else { //that is in this
          s match {
            case "lower" => Set(AInt(this.lb, that), AInt(AInt.binop(_ + _, that, Some(1)), this.ub))
            case "upper" => Set(AInt(this.lb, AInt.binop(_ - _, that, Some(1))), AInt(that, this.ub))
            case "neither" => Set(AInt(this.lb, AInt.binop(_ - _, that, Some(1))), AInt(AInt.binop(_ + _, that, Some(1)), this.ub))
          }
        }
    }
  }

  //(same part, part that's different from that)
  //TODO what happens if one value is None
  def ===(that: AInt): (Set[AInt], Set[AInt]) = {
    that match {
      case that: AInt =>
        if ((AInt.<(this.lb, that.lb) && AInt.<(this.ub, that.lb)) || (AInt.<(that.ub, this.lb) && AInt.<(that.ub, this.ub))) (Set(), Set(this)) //that is not in this
        else if (this == that) (Set(this), Set()) //same intervals
        else { //this and that have equal parts

          if (this.lb == that.lb) {
            if(that.ub == None){
              (Set(this), Set())
            }else if(this.lb == None){
              (Set(this), Set())
            } else if (AInt.<(this.ub, that.ub)) {
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set())
            } else { // if(AInt.<(that.ub, this.ub)){
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set(AInt(AInt.binop(_ + _, that.ub, Some(1)), this.ub)))
            }
          } else if (AInt.<(this.lb, that.lb)) {
            if(that.ub == None){
             (Set(this), Set())
            }else if(this.lb == None){
              (Set(this), Set())
            } else if (this.ub == that.ub) {
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set(AInt(this.lb, AInt.binop(_ - _, that.lb, Some(1)))))
            } else if (AInt.<(this.ub, that.ub)) {
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set(AInt(this.lb, AInt.binop(_ - _, that.lb, Some(1)))))
            } else { //if(AInt.<(that.ub, this.ub))
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set(AInt(this.lb, AInt.binop(_ - _, that.lb, Some(1))), AInt(AInt.binop(_ + _, that.ub, Some(1)), this.ub)))

            }
          } else { //if(AInt.<(that.lb, this.lb))
            if(that.ub == None){
              (Set(this), Set())
            }else if(this.lb == None){
              (Set(this), Set())
            } else if(this.ub == that.ub) {   //TODO add that.ub == None and this.ub == None
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set())
            } else if (AInt.<(this.ub, that.ub)) {
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set())
            } else { //if(AInt.<(that.ub, this.ub))
              (Set(this.intersect(that).justValue().asInstanceOf[AInt]), Set(AInt(AInt.binop(_ + _, that.ub, Some(1)), this.ub)))
            }
          }
        }
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
      case (ASome(a), ASome(b)) => ASome(a widen b)
    }
  }
}

case object ANone extends AOption

case class ASome(get: AVal) extends AOption

case class AMaybe(get: AVal) extends AOption

sealed trait AList extends AVal {

  def flatten: List[AVal]

  def flatten_All(): AList = this match {
    case ANil => ANil
    case ACons(h, t) => AMany(h).union(t)
    case AMany(e) => AMany(e)
  }

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


  def union(that: AVal): AList = {
    (this, that) match {
      case (ANil, ANil) => ANil

      case (ANil, ACons(a, as)) => ACons(a, as)
      case (ACons(a, as), ANil) => ACons(a, as)

      case (ANil, AMany(e)) => AMany(e)
      case (AMany(e), ANil) => AMany(e)

      case (ACons(a, as), AMany(e)) => AMany(a.asInstanceOf[AInt].union(e)).union(as)
      case (AMany(e), ACons(a, as)) => AMany(e.asInstanceOf[AInt].union(a)).union(as)

      case (AMany(e1), AMany(e2)) => AMany(e1.asInstanceOf[AInt].union(e2))
      case (ACons(a, as), ACons(b, bs)) => ACons(a.asInstanceOf[AInt].union(b), as.union(bs))
    }
  }

  def intersect(that: AVal): AList = {
    (this, that) match {
      case (ANil, ANil) => ANil

      case (ANil, ACons(_, _)) => ANil
      case (ACons(_, _), ANil) => ANil

      case (ANil, AMany(_)) => ANil
      case (AMany(_), ANil) => ANil

      case (ACons(a, as), AMany(e)) => if (a.asInstanceOf[AInt].intersect(e) != ANone) ACons(a.asInstanceOf[AInt].intersect(e).justValue(), as.intersect(AMany(e))) else ANil
      case (AMany(e), ACons(a, as)) => if (e.asInstanceOf[AInt].intersect(a) != ANone) ACons(e.asInstanceOf[AInt].intersect(a).justValue(), AMany(e).intersect(as)) else ANil

      case (AMany(e1), AMany(e2)) => if (e1.asInstanceOf[AInt].intersect(e2) != ANone) AMany(e1.asInstanceOf[AInt].intersect(e2).justValue()) else ANil
      case (ACons(a, as), ACons(b, bs)) => if (a.asInstanceOf[AInt].intersect(b) != ANone) ACons(a.asInstanceOf[AInt].intersect(b).justValue(), as.intersect(bs)) else ANil
    }
  }


  //is that a subset of this
  def subset(that: AVal): ABool = {
    (this, that) match {
      case (ANil, ANil) => ATrue

      case (ANil, ACons(_, _)) => AFalse
      case (ACons(_, _), ANil) => ATrue

      case (ANil, AMany(_)) => ATrue
      case (AMany(_), ANil) => ATrue

      case (ACons(a, as), AMany(e)) => a.asInstanceOf[AInt].contains(e) && as.subset(AMany(e))
      case (AMany(e), ACons(a, as)) => e.asInstanceOf[AInt].contains(a) && AMany(e).subset(as)

      case (AMany(e1), AMany(e2)) => e1.asInstanceOf[AInt].contains(e2)
      case (ACons(a, as), ACons(b, bs)) => a.asInstanceOf[AInt].contains(b) && as.subset(bs)
    }
  }


  //reverses an AList value
  def reverse(): AList = {
    this match {
      case ANil => ANil
      case AMany(e) => AMany(e)
      case ACons(_, _) =>
        var axs: AList = this
        var ays: AList = ANil
        var tailIsAMany = false
        val testCond = APred("isNil", "xs")
        var state = AState(Map(("xs", axs)))

        while (testCond.positive(axs).isEmpty && testCond.negative(axs).nonEmpty && !tailIsAMany) { //while !isNil
          val head = AOp("aHead", List(AVar("xs"))).evaluate(state).justValue()
          val tail = AOp("aTail", List(AVar("xs"))).evaluate(state).justValue()

          if (testCond.positive(tail).isEmpty && testCond.negative(tail).nonEmpty || tail == ANil) { //tail is not AMany but can be ACons or ANil
            ays = ACons(head, ays)
            axs = tail.asInstanceOf[AList]
            state = state.updated("xs", axs)
          } else if (testCond.positive(tail).nonEmpty && testCond.negative(tail).nonEmpty) { //tail is AMany
            ays = this.flatten_All()
            tailIsAMany = true
          } else {
            throw new Exception("Exception thrown from reverse.")
          }
        }
        ays
    }
  }


  //prepends an element on the front a an AList value
  def +:(elem: AInt, al2: AList): AList = ACons(elem, al2)

  //appends an element the the end a an AList value
  def :+(al1: AList, elem: AInt): AList = al1 match {
    case ANil => ACons(elem, ANil)
    case ACons(h, t) => ACons(h, :+(t, elem))
    case AMany(e) => AMany(e.asInstanceOf[AInt].union(elem))
  }

  //Method concatenates two values of type AList
  def ++(al1: AList, al2: AList): AList = (al1, al2) match {
    case (ANil, ANil) => ANil
    case (ANil, ACons(h, t)) => ACons(h, t)
    case (ACons(h, t), ANil) => ACons(h, t)
    case (ANil, AMany(e)) => AMany(e)
    case (AMany(e), ANil) => AMany(e)
    case (AMany(e1), AMany(e2)) => AMany(e1.asInstanceOf[AInt].union(e2))
    case (AMany(_), ACons(_, _)) => al1.widen(al2) //TODO test
    case (ACons(h, t), AMany(_)) => ACons(h, ++(t, al2))
    case (ACons(h1, t1), ACons(_, _)) => ACons(h1, ++(t1, al2))
  }

}

case object ANil extends AList {
  def flatten: List[AVal] = Nil
}

case class ACons(head: AVal, tail: AList) extends AList {
  def flatten: List[AVal] = head :: tail.flatten
}

case class AMany(elems: AVal) extends AList {
  def flatten: List[AVal] = List(elems)

}


