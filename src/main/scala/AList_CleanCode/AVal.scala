package AList_CleanCode

sealed trait AVal {
  def hasConcreteElement(that: Any): Boolean

  def widen(that: AVal): AVal

  def widen(that: List[AVal]): AVal = {
    that match {
      case Nil => this
      case first :: rest => (this widen first) widen rest
    }
  }

  def union(that: AVal): AVal

  def intersect(that: AVal): AVal
}

sealed trait ABool extends AVal {

  def hasConcreteElement(that: Any): Boolean = {
    (this, that) match {
      case (ATrue, true) | (AFalse, false) => true
      case (ATrue, false) | (AFalse, true) => false
      case (AUnknown, true) | (AUnknown, false) => true
    }
  }

  def widen(that: AVal): ABool = {
    (this, that) match {
      case (ATrue, ATrue) => ATrue
      case (AFalse, AFalse) => AFalse
      case (_: ABool, _: ABool) => AUnknown
    }
  }

  def eq(that: AVal): ABool = {
    (this, that) match {
      case (AFalse, AFalse) | (ATrue, ATrue) => ATrue
      case (AFalse, ATrue) | (ATrue, AFalse) => AFalse
      case (_: ABool, _: ABool) => AUnknown
    }
  }

  def noneq(that: AVal): ABool = {
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

  def ! : ABool = {
    this match {
      case AFalse => ATrue
      case ATrue => AFalse
      case AUnknown => AUnknown
    }
  }

  def union(that: AVal): ABool = widen(that)

  def intersect(that: AVal): ABool = {
    (this, that) match {
      case (ATrue, ATrue) | (AUnknown, ATrue) | (ATrue, AUnknown) => ATrue
      case (AFalse, AFalse) | (AUnknown, AFalse) | (AFalse, AUnknown) => AFalse
      case (_: ABool, _: ABool) => AUnknown
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
      case (None, _) => true
      case (Some(a), Some(b)) => a < b
    }
  }

  def <=(a: Option[Int], b: Option[Int]): Boolean = {
    (a, b) match {
      case (_, None) => false
      case (None, _) => true
      case (Some(a), Some(b)) => a < b || a == b
    }
  }
}

case class AInt(lb: Option[Int], ub: Option[Int]) extends AVal {

  def hasConcreteElement(that: Any): Boolean = {
    (lb, ub, that) match {
      case (None, None, i: Int) => true
      case (None, Some(e), i: Int) => i <= e
      case (Some(e), None, i: Int) => i >= e
      case (Some(e1), Some(e2), i: Int) => i >= e1 && i <= e2
    }
  }

  def widen(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val lb = if (AInt.<(that.lb, this.lb) || that.lb == None) None else this.lb
        val ub = if (AInt.<(this.ub, that.ub) || that.ub == None) None else this.ub
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

  def unary_- : AInt = {
    (lb, ub) match {
      case (None, None) => this
      case (lb, None) => AInt(None, Some(-lb.get))
      case (None, ub) => AInt(Some(-ub.get), None)
      case _ => AInt(Some(-ub.get), Some(-lb.get))
    }
  }

  def abs: AInt = {
    (lb, ub) match {
      case (None, None) => AInt(Some(0), None)
      case (_, None) => AInt(Some(lb.get.abs), None)
      case (None, _) => AInt(Some(ub.get.abs), None)
      case _ =>
        val newlb = Some(lb.get.abs)
        val newub = Some(ub.get.abs)
        if (AInt.<=(newlb, newub)) AInt(newlb, newub) else AInt(newub, newlb)
    }
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

  def <(that: AVal): Boolean = {
    that match {
      case that: AInt =>
        AInt.<(this.lb, that.lb) && AInt.<(this.ub, that.ub)
    }
  }

  def <=(that: AVal): Boolean = {
    that match {
      case that: AInt =>
        AInt.<=(this.lb, that.lb) && AInt.<=(this.ub, that.ub)
    }
  }

  def union(that: AVal): AInt = {
    that match {
      case that: AInt =>
        val newlb = if (AInt.<(this.lb, that.lb)) this.lb else that.lb
        val newub = if (this.ub == None || that.ub == None) None
        else if (AInt.<(this.ub, that.ub)) that.ub
        else this.ub
        AInt(newlb, newub)
    }
  }


  def intersect(that: AVal): AInt = {
    that match {
      case that: AInt =>
        //if that has specified boundaries and the current interval this is not a subset of that
        if ((this.lb != None && that.lb != None && this.ub != None && that.ub != None) && (AInt.<(this.lb, that.lb) && AInt.<(this.ub, that.lb))) {
          AInt(Some(0), Some(-1))

        //if that has specified boundaries and the current interval this is not a subset of that
        }else if ((this.lb != None && that.lb != None && this.ub != None && that.ub != None) && (AInt.<(that.ub, this.lb) && AInt.<(that.ub, this.ub))){
          AInt(Some(0), Some(-1))
        }
        else {
          val newlb = if (AInt.<=(this.lb, that.lb) || this.lb == None) that.lb else this.lb
          val newub = if (this.ub == None) that.ub else if (that.ub == None) this.ub else if (AInt.<=(this.ub, that.ub)) this.ub else that.ub
          AInt.apply(newlb, newub)
        }
    }
  }


  //checks which parts of two intervals are the same (same part, part of this which is different from that)
  def comp(that: AVal): (Set[AVal], Set[AVal]) = {
    that match {
      case that: AInt =>
        //no equal parts
        if ((that.lb != None && that.ub != None) && ((AInt.<(lb, that.lb) && AInt.<(ub, that.lb)) || (AInt.<(that.ub, lb) && AInt.<(that.ub, ub)))) {
          (Set(), Set(this))
        } else if ((that.lb != None && that.ub == None) && (lb != None && ub != None) && (AInt.<(lb, that.lb) && AInt.<(ub, that.lb))) {
          (Set(), Set(this))
        } else if ((that.ub != None && that.lb == None) && (lb != None && ub != None) && (AInt.<(that.ub, lb) && AInt.<(that.ub, ub))) {
          (Set(), Set(this))

          //same intervals
        } else if (this == that) {
          (Set(this), Set())
          //this == AInt.top
        } else if (this == AInt.top) {
          if (that.lb == None) (Set(AInt(None, that.ub)), Set(AInt(AInt.binop(_ + _, that.ub, Some(1)), None)))
          else if (that.ub == None) (Set(AInt(that.lb, None)), Set(AInt(None, AInt.binop(_ - _, that.lb, Some(1)))))
          else (Set(that), Set(AInt(None, AInt.binop(_ - _, that.lb, Some(1))), AInt(AInt.binop(_ + _, that.ub, Some(1)), None)))
        }
        else { //this and that have equal parts
          val intersect = this.intersect(that)

          if (this.lb == that.lb) {
            if (that == AInt.top || that.ub == None) {
              (Set(this), Set())
            } else if (AInt.<(this.ub, that.ub)) {
              (Set(intersect), Set())
            } else { // if(AInt.<=(that.ub, this.ub)){
              (Set(intersect), Set(AInt(AInt.binop(_ + _, that.ub, Some(1)), this.ub)))
            }

          } else if (AInt.<(that.lb, lb)) {
            if ((that == AInt.top) || (that.ub == None || lb == None)) {
              (Set(this), Set())
            } else if (AInt.<=(ub, that.ub)) {
              (Set(intersect), Set())
            } else { //if(AInt.<(that.ub, this.ub))
              (Set(intersect), Set(AInt(AInt.binop(_ + _, that.ub, Some(1)), ub)))
            }
          } else { //if (AInt.<(lb, that.lb))
            if (that.ub == None) {
              (Set(AInt(that.lb, ub)), Set(AInt(lb, AInt.binop(_ - _, that.lb, Some(1)))))
            } else if (AInt.<=(ub, that.ub)) {
              (Set(intersect), Set(AInt(lb, AInt.binop(_ - _, that.lb, Some(1)))))
            } else { //if(AInt.<(that.ub, this.ub))
              (Set(intersect), Set(AInt(lb, AInt.binop(_ - _, that.lb, Some(1))), AInt(AInt.binop(_ + _, that.ub, Some(1)), this.ub)))
            }
          }
        }
    }
  }

}


sealed trait AOption extends AVal {

  def hasConcreteElement(that: Any): Boolean = {
    (this, that) match {
      case (ANone, None) => true
      case (ANone, Some(e)) => false
      case (ASome(ae), None) => false
      case (ASome(ae), Some(e)) => ae.hasConcreteElement(e)
      case (AMaybe(ae), None) => true
      case (AMaybe(ae), Some(e)) => ae.hasConcreteElement(e)
    }
  }

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

  def union(that: AVal): AOption = {
    (this, that) match {
      case (ANone, ANone) => ANone
      case (ANone, ASome(a)) => AMaybe(a)
      case (ASome(a), ANone) => AMaybe(a)
      case (ANone, AMaybe(a)) => AMaybe(a)
      case (AMaybe(a), ANone) => AMaybe(a)
      case (AMaybe(a), ASome(b)) => AMaybe(a union b)
      case (ASome(a), AMaybe(b)) => AMaybe(a union b)
      case (AMaybe(a), AMaybe(b)) => AMaybe(a union b)
      case (ASome(a), ASome(b)) => ASome(a union b)
    }
  }

  def intersect(that: AVal): AOption = {
    (this, that) match {
      case (ANone, ANone) => ANone
      case (ANone, ASome(a)) => ANone
      case (ASome(a), ANone) => ANone
      case (ANone, AMaybe(b)) => ANone
      case (AMaybe(a), ANone) => ANone
      case (AMaybe(a), ASome(b)) => if (a.intersect(b) != ANone) ASome(a intersect b) else ANone
      case (ASome(a), AMaybe(b)) => if (a.intersect(b) != ANone) ASome(a intersect b) else ANone
      case (AMaybe(a), AMaybe(b)) => if (a.intersect(b) != ANone) AMaybe(a intersect b) else ANone
      case (ASome(a), ASome(b)) => if (a.intersect(b) != ANone) ASome(a intersect b) else ANone
    }
  }

  def get: AVal = {
    this match {
      case ASome(e) => e
      case _ => throw new Exception("Exception thrown from AOption.get. Value was not ASome")
    }
  }
}

case object ANone extends AOption
case class ASome(elem: AVal) extends AOption
case class AMaybe(elem: AVal) extends AOption

sealed trait AList extends AVal {

  def length: AInt
  def headOption: AOption
  def tailOption: AOption
  def flatten: List[AVal]

  def hasConcreteElement(that: Any): Boolean = {
    (this, that) match {
      case (ANil, List()) => true
      case (ANil, list: List[Any]) => false
      case (ACons(h, t), List()) => false
      case (ACons(h, t), list: List[Any]) => h.hasConcreteElement(list.head) && t.hasConcreteElement(list.tail)
      case (AMany(ae), List()) => true
      case (AMany(ae), list: List[Any]) =>
        var hasElem = ae.hasConcreteElement(list.head)
        for (l <- list.tail) hasElem = hasElem && ae.hasConcreteElement(l)
        hasElem
    }
  }

  def widen(that: AVal): AList = {
    (this, that) match {
      case (ANil, ANil) => ANil
      case (ANil, ACons(a, as)) => AMany(a widen as.flatten)
      case (ANil, AMany(e)) => AMany(e)
      case (ACons(a, as), ANil) => AMany(a widen as.flatten)
      case (ACons(a, as), AMany(e)) => AMany((a widen as.flatten) widen e)
      case (ACons(a, as), ACons(b, bs)) => ACons(a widen b, as widen bs)
      case (AMany(e), ANil) => AMany(e)
      case (AMany(e), ACons(a, as)) => AMany(e widen (a widen as.flatten))
      case (AMany(e1), AMany(e2)) => AMany(e1 widen e2)
    }
  }

  def intersect(that: AVal): AList = {
    (this, that) match {
      case (ANil, ANil) => ANil
      case (ANil, ACons(_, _)) => ACons(AInt(Some(0), Some(-1)), ANil)
      case (ACons(a, as), ANil) => ACons(AInt(Some(0), Some(-1)), ANil)
      case (ANil, AMany(_)) => ANil
      case (AMany(_), ANil) => ANil
      case (ACons(a, as), AMany(e)) => if (a.intersect(e) != AInt(Some(0), Some(-1))) ACons(a.intersect(e), as.intersect(that)) else ACons(AInt(Some(0), Some(-1)), ANil)
      case (AMany(e), ACons(a, as)) => if (e.intersect(a) != AInt(Some(0), Some(-1))) ACons(e.intersect(a), this.intersect(as)) else ACons(AInt(Some(0), Some(-1)), ANil)
      case (AMany(e1), AMany(e2)) => if (e1.intersect(e2) != AInt(Some(0), Some(-1))) AMany(e1.intersect(e2)) else ACons(AInt(Some(0), Some(-1)), ANil)
      case (ACons(a, as), ACons(b, bs)) => if (a.intersect(b) != AInt(Some(0), Some(-1))) ACons(a.intersect(b), as.intersect(bs)) else ACons(AInt(Some(0), Some(-1)), ANil)
    }
  }

  def union(that: AVal): AList = concat(that)

  //reverses an AList value
  def reverse(): AList = {
    this match {
      case ANil => ANil
      case AMany(e) => AMany(e)
      case ACons(h, t) =>
        var axs : AList  = this
        var ays: AList = ANil
        var tailIsAMany = false //to avoid an endless loop

        val test = APred("isNil", "xs")
        var state = AState(Map("xs" -> axs))

        while (test.positive(axs).isEmpty && test.negative(axs).nonEmpty && !tailIsAMany) { //while !isNil

          val head  = AOp("get", List(AConst(axs.headOption))).evaluate(state)
          val tail  = AOp("get", List(AConst(axs.tailOption))).evaluate(state).asInstanceOf[AList]

          if (test.positive(tail).isEmpty && test.negative(tail).nonEmpty || tail == ANil) { //tail is not AMany but can be ACons or ANil
            ays = ACons(head, ays)
            axs = tail
            state = state.updated("xs", axs)
          } else if (test.positive(tail).nonEmpty && test.negative(tail).nonEmpty) { //tail is AMany
            ays = AMany(h).concat(t) // this.flatten_All
            tailIsAMany = true
          } else {
            throw new Exception("Exception thrown from reverse.")
          }
        }
        ays
    }
  }


  //prepends an element on the front a an AList value
  def prepend(elem: AVal): AList = {
    this match {
      case ANil => ACons(elem, ANil)
      case ACons(a,as) => ACons(elem, this)
      case AMany(e) => AMany(elem.union(e))
    }
  }

  //appends an element the the end a an AList value
  def append(elem: AVal): AList =
    this match {
      case ANil => ACons(elem, ANil)
      case ACons(h, t) => ACons(h, t.append(elem))
      case AMany(e) => AMany(e.union(elem))
    }

  //Method concatenates two values of type AList
  def concat(that: AVal): AList =
    (this, that) match {
      case (ANil, ANil) => ANil
      case (ANil, ACons(h, t)) => ACons(h, t)
      case (ANil, AMany(e)) => AMany(e)
      case (AMany(e), ANil) => AMany(e)
      case (AMany(e1), AMany(e2)) => AMany(e1.union(e2))
      case (AMany(e), ACons(h, t)) => AMany(e.union(h)).concat(t)
      case (ACons(h, t), ANil) => ACons(h, t)
      case (ACons(h, t), AMany(_)) => ACons(h, t.concat(that))
      case (ACons(h1, t1), ACons(_, _)) => ACons(h1, t1.concat(that))
    }
}

case object ANil extends AList {
  def headOption: AOption = ANone
  def tailOption: AOption = ANone
  def flatten: List[AVal] = Nil
  def length: AInt = AInt.zero
}

case class ACons(hd: AVal, tl: AList) extends AList {
  def headOption: AOption = ASome(hd)
  def tailOption: AOption = ASome(tl)
  def flatten: List[AVal] = hd :: tl.flatten
  def length: AInt = AInt(Some(1), None)
}

case class AMany(elems: AVal) extends AList {
  def headOption: AOption = AMaybe(elems)
  def tailOption: AOption = AMaybe(AMany(elems))
  def flatten: List[AVal] = List(elems)
  def length: AInt = AInt(Some(0), None)
}


