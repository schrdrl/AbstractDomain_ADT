package AList

import Console.{GREEN, RED, RESET}

/**
 * AList is an abstract domain of numerical lists (belonging to algebraic data types)
 * It is described by an interval and has three types:
 * -ANil: describes the empty AList
 * -ACons(h,t): has a specified head with associated interval and a tail of type AList
 * -AMany(e): contains both types ANil and ACons
 */
case class ALists(intervals: Intervals) {
  import intervals.Interval //import inner Class
  type AInt = Interval //alias

  sealed trait AList  //"behaviour"
  case object ANil extends AList
  case class ACons(aHead: AInt, aTail: AList) extends AList
  case class AMany(elem: AInt) extends AList

  sealed trait AOption[+A]
  case object ANone extends AOption[Nothing]
  case class ASome[A](get: A) extends AOption[A]
  case class AMaybe[A](get: A) extends AOption[A]

  sealed trait ABool
  case object ATrue extends ABool
  case object AFalse extends ABool
  //case object AUnknown extends ABool

  /************************************************************
   *                 Basic Functions over ALists              *
   ************************************************************/

  //returns the head of aList object, which is of type AOption[AInt]
  def aHead(l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(h, _) => ASome(h)
    case AMany(e) => AMaybe(e) //AMany = ANil ≀ ACons(e, Many(e))
  }

  //returns the tail of aList object, which is of type AOption[AInt]
  def aTail(l: AList): AOption[AList] = l match {
    case ANil => ANone
    case ACons(_, t) => ASome(t)
    case AMany(_) => AMaybe(l) //l instead of AMany(e)
  }

  //returns the length of aList object, which is of type AOption[AInt]
  def aLength(l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(_, _) => ASome(Interval(IntegerVal(1), IntegerInf))
    case AMany(_) => ASome(Interval(IntegerVal(0), IntegerInf))
  }

  // checks whether a given AList is Nil
  def isNil(l: AList): Set[ABool] = l match {
    case ANil => Set(ATrue)
    case ACons(_, _) => Set(AFalse)
    case AMany(_) => Set(ATrue, AFalse)
  }

  /************************************************************
   *                   isConcreteElementOf                    *
   ************************************************************/

  //checks whether an integer value is a concrete Element of an Interval.
  def isConcreteElementOf_Int(i: Int, ai: AInt): Boolean = {
    intervals.contains(ai, i)
  }

  //checks whether a list is a concrete Element of AList.
  def isConcreteElementOf_List(l: List[Int], al: AList): Boolean = (l, al) match {
    case (Nil, ANil) => true
    case (Nil, ACons(_, _)) => false
    case (Nil, AMany(_)) => true //AMany = ANil ≀ ACons(e, Many(e))
    case (x :: xs, ANil) => false
    case (x :: xs, ACons(ax, axs)) => isConcreteElementOf_Int(x, ax) && isConcreteElementOf_List(xs, axs)
    case (x :: xs, AMany(ax)) =>
      isConcreteElementOf_Int(x, ax) && isConcreteElementOf_List(xs, al)
  }

  //checks whether an Option[Int] is a concrete Element of an interval AOption[AInt].
  def isConcreteElementOf_OptionInt(o: Option[Int], ao: AOption[AInt]): Boolean = (o, ao) match {
    case (None, ANone) => true
    case (None, ASome(_)) => false
    case (None, AMaybe(_)) => true
    case (Some(_), ANone) => false
    case (Some(h1), ASome(h2)) => intervals.contains(h2, h1)
    case (Some(h1), AMaybe(h2)) => intervals.contains(h2, h1)
  }

  //checks whether an Option[List[Int]] is a concrete Element of an interval AOption[AList].
  def isConcreteElementOf_OptionList(o: Option[List[Int]], ao: AOption[AList]): Boolean = (o, ao) match {
    case (None, ANone) => true
    case (None, ASome(_)) => false
    case (None, AMaybe(_)) => true
    case (Some(_), ANone) => false
    case (Some(h1), ASome(h2)) => isConcreteElementOf_List(h1, h2)
    case (Some(h1), AMaybe(h2)) => isConcreteElementOf_List(h1, h2)
  }

  //checks whether a Boolean value is a concrete Element of an ABool.
  def isConcreteElementOf_ABool(b: Boolean, ab: ABool): Boolean = (b, ab) match {
    case (true, ATrue) => true
    case (false, AFalse) => true
    case (false, ATrue) | (true, AFalse) => false
  }


  /************************************************************
   *                Advanced Functions over ALists            *
   ************************************************************/
   //TODO recheck
   //Method prepends an element on the front a an AList value
   def +:(elem: AInt, al2: AList) : AList = ACons(elem, al2 )

  //TODO recheck
  //Method appends an element the the end a an AList value
   def :+(al1: AList, elem: AInt) : Set[AList] = al1 match {
     case ANil => Set(ACons(elem, ANil))
     case ACons(h,t) => if(:+(t, elem).tail.nonEmpty) Set(ACons(h, :+(t, elem).head), ACons(h, :+(t, elem).tail.head)) else Set(ACons(h, :+(t, elem).head))//TODO recheck
     case AMany(e) => Set(ACons(elem, ANil), ACons(e, AMany(intervals.union_Interval(e, elem))))  //TODO recheck
     //1. case: ANil -> ACons(elem, ANil)
     //2. case: ACons(e, AMany(e)) -> ACons(e, ACons(elem, ANil)) oder ACons(e, ACons(elem, AMany(e))) oder ACons(e, AMany(e union elem)) ???
   }

  //TODO recheck
  //Method concatenates two values of type AList
   def ++(al1: AList, al2: AList): Set[AList] = (al1, al2) match {
     case (ANil, ANil) => Set(ANil)
     case (ANil, ACons(h,t)) => Set(al2)
     case (ACons(h,t), ANil) => Set(al1)
     case (ANil, AMany(e)) => Set(al2)
     case (AMany(e), ANil) => Set(al1)
     case (AMany(e1), AMany(e2)) => Set(AMany(intervals.union_Interval(e1,e2)))
     case (AMany(e1), ACons(h,t)) => Set(al2, ACons(e1, union_AList(al1, al2))) //TODO recheck
     case (ACons(h,t), AMany(e2)) =>if(++(t,ACons(e2,AMany(e2))).tail.nonEmpty) Set(ACons(h, ++(t,ACons(e2,AMany(e2))).head), ACons(h, ++(t,ACons(e2,AMany(e2))).tail.head)) else Set(al1, ACons(h,++(t,ACons(e2,AMany(e2))).head)) //TODO recheck
     case (ACons(h1, t1), ACons(h2, t2)) =>  if(++(t1, al2).tail.nonEmpty) Set(ACons(h1, ++(t1, al2).head),ACons(h1, ++(t1, al2).tail.head)) else Set(ACons(h1, ++(t1, al2).head))
   }


  //union of two ALists
  def union_AList(al1: AList, al2: AList): AList = (al1, al2) match {
    case (ANil, ANil) => ANil
    case (ANil, AMany(e)) => AMany(e)
    case (AMany(e), ANil) => AMany(e)
    case (ANil, ACons(a, as)) => union_AList(AMany(a), as)
    case (ACons(a, as), ANil) => union_AList(AMany(a), as)
    case (AMany(a), AMany(b)) => AMany(intervals.union_Interval(a, b))
    case (ACons(a, as), AMany(e)) => union_AList(AMany(intervals.union_Interval(a, e)), as)
    case (AMany(e), ACons(a, as)) => union_AList(AMany(intervals.union_Interval(a, e)), as)
    case (ACons(a, as), ACons(b, bs)) => ACons(intervals.union_Interval(a, b), union_AList(as, bs))
  }

  //union of two values of type AOption[AInt]
  def union_AOption_AInt(ao1: AOption[AInt], ao2: AOption[AInt]): AOption[AInt] = (ao1, ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, AMaybe(e)) => AMaybe(e)
    case (AMaybe(e), ANone) => AMaybe(e)
    case (ANone, ASome(e)) => AMaybe(e)
    case (ASome(e), ANone) => AMaybe(e)
    case (ASome(a), ASome(b)) => ASome(intervals.union_Interval(a, b))
    case (ASome(a), AMaybe(b)) => AMaybe(intervals.union_Interval(a, b))
    case (AMaybe(a), ASome(b)) => AMaybe(intervals.union_Interval(a, b))
    case (AMaybe(a), AMaybe(b)) => AMaybe(intervals.union_Interval(a, b))
  }

  //union of two values of type AOption[AList]
  def union_AOption_AList(ao1: AOption[AList], ao2: AOption[AList]): AOption[AList] = (ao1, ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, AMaybe(e)) => AMaybe(e)
    case (AMaybe(e), ANone) => AMaybe(e)
    case (ANone, ASome(e)) => AMaybe(e)
    case (ASome(e), ANone) => AMaybe(e)
    case (ASome(a), ASome(b)) => ASome(union_AList(a, b))
    case (ASome(a), AMaybe(b)) => AMaybe(union_AList(a, b))
    case (AMaybe(a), ASome(b)) => AMaybe(union_AList(a, b))
    case (AMaybe(a), AMaybe(b)) => AMaybe(union_AList(a, b))
  }

  //union of two values of type ABool
  def union_ABool(ab1: ABool, ab2: ABool): Set[ABool] = (ab1, ab2) match {
    case (AFalse, AFalse) => Set(AFalse)
    case (ATrue, ATrue) => Set(ATrue)
    case (ATrue, AFalse) | (AFalse, ATrue) => Set(AFalse, ATrue)
  }

  //intersection of two ALists
  def intersect_AList(al1: AList, al2: AList): AList = (al1, al2) match {
    case (ANil, ANil) => ANil
    case (ANil, AMany(_)) | (AMany(_), ANil) => ANil
    case (ANil, ACons(_, _)) | (ACons(_, _), ANil) => ANil
    case (AMany(a), AMany(b)) =>
      if (intervals.intersect_Interval(a, b) != intervals.Interval(IntegerInf, IntegerNegInf)) AMany(intervals.intersect_Interval(a, b)) else ANil
    case (ACons(a, as), AMany(e)) =>
      if (intervals.intersect_Interval(a, e) != intervals.Interval(IntegerInf, IntegerNegInf)) ACons(intervals.intersect_Interval(a, e), intersect_AList(as, al2)) else ANil
    case (AMany(e), ACons(a, as)) =>
      if (intervals.intersect_Interval(e, a) != intervals.Interval(IntegerInf, IntegerNegInf)) ACons(intervals.intersect_Interval(e, a), intersect_AList(al1, as)) else ANil
    case (ACons(a, as), ACons(b, bs)) =>
      if (intervals.intersect_Interval(a, b) != intervals.Interval(IntegerInf, IntegerNegInf)) ACons(intervals.intersect_Interval(a, b), intersect_AList(as, bs)) else ANil
  }

  //intersection of two values of type AOption[AInt]
  def intersect_AOption_AInt(ao1: AOption[AInt], ao2: AOption[AInt]): AOption[AInt] = (ao1, ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, AMaybe(_)) => ANone
    case (AMaybe(_), ANone) => ANone
    case (ANone, ASome(_)) => ANone
    case (ASome(_), ANone) => ANone
    case (ASome(a), ASome(b)) => if (intervals.intersect_Interval(a, b) != intervals.Interval(IntegerInf, IntegerNegInf)) ASome(intervals.intersect_Interval(a, b)) else ANone
    case (ASome(a), AMaybe(b)) => if (intervals.intersect_Interval(a, b) != intervals.Interval(IntegerInf, IntegerNegInf)) ASome(intervals.intersect_Interval(a, b)) else ANone
    case (AMaybe(a), ASome(b)) => if (intervals.intersect_Interval(a, b) != intervals.Interval(IntegerInf, IntegerNegInf)) ASome(intervals.intersect_Interval(a, b)) else ANone
    case (AMaybe(a), AMaybe(b)) => if (intervals.intersect_Interval(a, b) != intervals.Interval(IntegerInf, IntegerNegInf)) AMaybe(intervals.intersect_Interval(a, b)) else ANone
  }

  //intersection of two values of type AOption[AList]
  def intersect_AOption_AList(ao1: AOption[AList], ao2: AOption[AList]): AOption[AList] = (ao1, ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, AMaybe(e)) => ANone
    case (AMaybe(_), ANone) => ANone
    case (ANone, ASome(_)) => ANone
    case (ASome(_), ANone) => ANone
    case (ASome(a), ASome(b)) => ASome(intersect_AList(a, b))
    case (ASome(a), AMaybe(b)) => ASome(intersect_AList(a, b))
    case (AMaybe(a), ASome(b)) => ASome(intersect_AList(a, b))
    case (AMaybe(a), AMaybe(b)) => AMaybe(intersect_AList(a, b))
  }

  //intersection of two values of type ABool
  def intersect_ABool(ab1: ABool, ab2: ABool): Set[ABool] = (ab1, ab2) match {
    case (AFalse, AFalse) => Set(AFalse)
    case (ATrue, ATrue) => Set(ATrue)
    case (ATrue, AFalse) | (AFalse, ATrue) => Set()
  }

  //checks whether the left AList(first parameter) is a subset of the right AList(second parameter)
  def subset_AList(al1: AList, al2: AList): Boolean = (al1, al2) match {
    case (ANil, ANil) => true
    case (ANil, AMany(_)) => true
    case (AMany(_), ANil) => true
    case (ANil, ACons(_, _)) => true
    case (ACons(_, _), ANil) => false
    case (AMany(a), AMany(b)) => intervals.contains_Interval(a, b)
    case (ACons(a, as), AMany(e)) => intervals.contains_Interval(a, e) && subset_AList(as, al2)
    case (AMany(e), ACons(a, as)) => intervals.contains_Interval(e, a) && subset_AList(al1, as)
    case (ACons(a, as), ACons(b, bs)) => intervals.contains_Interval(a, b) && subset_AList(as, bs)
  }


  //widening of two ALists (operation is not symmetric)
  def widen_AList(al1: AList, al2: AList): AList = (al1, al2) match {
    case (ANil, ANil) => ANil
    case (ANil, AMany(e)) => AMany(e)
    case (AMany(e), ANil) => AMany(e)
    case (ANil, ACons(a, as)) => widen_AList(AMany(a), as)
    case (ACons(a, as), ANil) => widen_AList(AMany(a), as)
    case (AMany(a), AMany(b)) => AMany(intervals.Lattice.widen(a, b))
    case (AMany(e), ACons(b, bs)) => widen_AList(AMany(intervals.Lattice.widen(e, b)), bs)
    case (ACons(a, as), AMany(e)) => widen_AList(AMany(intervals.Lattice.widen(a, e)), as)
    case (ACons(a, as), ACons(b, bs)) => ACons(intervals.Lattice.widen(a, b), widen_AList(as, bs))
  }

  //widening of two values of type AOption[AInt] (operation is not symmetric)
  def widen_AOptionAInt(ao1: AOption[AInt], ao2: AOption[AInt]): AOption[AInt] = (ao1, ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, ASome(e)) => AMaybe(e)
    case (ASome(e), ANone) => AMaybe(e)
    case (ANone, AMaybe(e)) => AMaybe(e)
    case (AMaybe(e), ANone) => AMaybe(e)
    case (AMaybe(a), AMaybe(b)) => AMaybe(intervals.Lattice.widen(a, b))
    case (ASome(a), ASome(b)) => ASome(intervals.Lattice.widen(a, b))
    case (AMaybe(a), ASome(b)) => AMaybe(intervals.Lattice.widen(a, b))
    case (ASome(a), AMaybe(b)) => AMaybe(intervals.Lattice.widen(a, b))
  }

  //widening of two values of type AOption[AList] (operation is not symmetric)
  def widen_AOptionAList(ao1: AOption[AList], ao2: AOption[AList]): AOption[AList] = (ao1, ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, ASome(e)) => AMaybe(e)
    case (ASome(e), ANone) => AMaybe(e)
    case (ANone, AMaybe(e)) => AMaybe(e)
    case (AMaybe(e), ANone) => AMaybe(e)
    case (AMaybe(a), AMaybe(b)) => AMaybe(widen_AList(a,b))
    case (ASome(a), ASome(b)) => ASome(widen_AList(a,b))
    case (AMaybe(a), ASome(b)) => AMaybe(widen_AList(a,b))
    case (ASome(a), AMaybe(b)) => AMaybe(widen_AList(a,b))
  }


  /************************************************************
   *          Operators (&&, ===, !==) returning ABool        *
   ************************************************************/


   def !(a: ABool): ABool = a match {
     case ATrue => AFalse
     case AFalse => ATrue
   }

  def &&(a: ABool, b: ABool): ABool = (a, b) match {
    case (ATrue, ATrue) => ATrue
    case (AFalse, AFalse) | (AFalse, ATrue) | (ATrue, AFalse) => AFalse
  }

  def ||(a: ABool, b: ABool): ABool = (a, b) match {
    case (ATrue, ATrue) | (AFalse, ATrue) | (ATrue, AFalse) => ATrue
    case (AFalse, AFalse)  => AFalse
  }

  def !==(ao1: ABool, ao2: ABool): ABool = (ao1, ao2) match {
    case (AFalse, AFalse) | (ATrue, ATrue) => AFalse
    case (AFalse, ATrue) | (ATrue, AFalse) => ATrue
  }

  def ===(a: ABool, b: ABool): ABool = (a, b) match {
    case (AFalse, AFalse) | (ATrue, ATrue) => ATrue
    case (AFalse, ATrue) | (ATrue, AFalse) => AFalse
  }

  def ===(a: AInt, b: AInt): ABool = {
    if (intervals.Lattice.<=(a,b) && intervals.Lattice.<=(b,a)){
      ATrue
    } else {
      AFalse
    }
  }


  def ===(l1: AList, l2: AList): ABool = (l1, l2) match {
    case (ANil, ANil) => ATrue
    case (ANil, ACons(_, _)) | (ACons(_, _), ANil) => AFalse
    case (ANil, AMany(_)) | (AMany(_), ANil) => ATrue
    case (ACons(a, as), ACons(b, bs)) => &&(===(a, b), ===(as, bs))
    case (AMany(a), AMany(b)) => ===(a, b)
    case (AMany(_), ACons(_, ANil)) => AFalse
    case (ACons(_, ANil) , AMany(_)) => AFalse
    case (AMany(_), ACons(_, _)) => AFalse
    case (ACons(_, _), AMany(_)) => AFalse
  }

  def ===(ao1: AOption[AInt], ao2: AOption[AInt]): ABool = (ao1, ao2) match {
    case (ANone, ANone) => ATrue
    case (ANone, ASome(_)) | (ASome(_), ANone) => AFalse
    case (ANone, AMaybe(_)) | (AMaybe(_), ANone) => ATrue
    case (ASome(a), ASome(b)) => ===(a, b)
    case (AMaybe(a), AMaybe(b)) => ===(a, b)
    case (ASome(a), AMaybe(b)) => ===(a, b)
    case (AMaybe(a), ASome(b)) => ===(a, b)
  }

  def ===(a1: AOption[AList], a2: AOption[AList]) (implicit d: DummyImplicit): ABool = (a1, a2) match {
    case (ANone, ANone) => ATrue
    case (ANone, ASome(_)) | (ASome(_), ANone) => AFalse
    case (ANone, AMaybe(_)) | (AMaybe(_), ANone) => ATrue
    case (ASome(a), ASome(b)) => ===(a, b)
    case (AMaybe(a), AMaybe(b)) => ===(a, b)
    case (ASome(a), AMaybe(b)) => ===(a, b)
    case (AMaybe(a), ASome(b)) => ===(a, b)
  }


  def AStateEqual(as1: AState_Base, as2: AState_Base): ABool = {
    as1.first match {
      case int: AInt =>
        as1.second match {
          case int1: AInt =>
            &&(===(int, as2.first.asInstanceOf[AInt]), ===(int1, as2.second.asInstanceOf[AInt]))
          case list: AList =>
            &&(===(int, as2.first.asInstanceOf[AInt]), ===(list, as2.second.asInstanceOf[AList]))
          case bool: ABool =>
            &&(===(int, as2.first.asInstanceOf[AInt]), ===(bool, as2.second.asInstanceOf[ABool]))
          case ao1: AOption[AInt] =>
            &&(===(int, as2.first.asInstanceOf[AInt]), ===(ao1, as2.second.asInstanceOf[AOption[AInt]]))
          case a1: AOption[AList] =>
            &&(===(int, as2.first.asInstanceOf[AInt]), ===(a1, as2.second.asInstanceOf[AOption[AList]]))
          case _ =>
            AFalse //if it is a different combination
        }
      case list: AList =>
        as1.second match {
          case int: AInt =>
            &&(===(list, as2.first.asInstanceOf[AList]), ===(int, as2.second.asInstanceOf[AInt]))
          case list1: AList =>
            &&(===(list, as2.first.asInstanceOf[AList]), ===(list1, as2.second.asInstanceOf[AList]))
          case bool: ABool =>
            &&(===(list, as2.first.asInstanceOf[AList]), ===(bool, as2.second.asInstanceOf[ABool]))
          case ao1: AOption[AInt] =>
            &&(===(list, as2.first.asInstanceOf[AList]), ===(ao1, as2.second.asInstanceOf[AOption[AInt]]))
          case a1: AOption[AList] =>
            &&(===(list, as2.first.asInstanceOf[AList]), ===(a1, as2.second.asInstanceOf[AOption[AList]]))
          case _ =>
            AFalse //if it is a different combination
        }
      case bool: ABool =>
        as1.second match {
          case int: AInt =>
            &&(===(bool, as2.first.asInstanceOf[ABool]), ===(int, as2.second.asInstanceOf[AInt]))
          case list: AList =>
            &&(===(bool, as2.first.asInstanceOf[ABool]), ===(list, as2.second.asInstanceOf[AList]))
          case bool1: ABool =>
            &&(===(bool, as2.first.asInstanceOf[ABool]), ===(bool1, as2.second.asInstanceOf[ABool]))
          case ao1: AOption[AInt] =>
            &&(===(bool, as2.first.asInstanceOf[ABool]), ===(ao1, as2.second.asInstanceOf[AOption[AInt]]))
          case a1: AOption[AList] =>
            &&(===(bool, as2.first.asInstanceOf[ABool]), ===(a1, as2.second.asInstanceOf[AOption[AList]]))
          case _ =>
            AFalse //if it is a different combination
        }
      case ao1: AOption[AInt] =>
        as1.second match {
          case int: AInt =>
            &&(===(ao1, as2.first.asInstanceOf[AOption[AInt]]), ===(int, as2.second.asInstanceOf[AInt]))
          case list: AList =>
            &&(===(ao1, as2.first.asInstanceOf[AOption[AInt]]), ===(list, as2.second.asInstanceOf[AList]))
          case bool: ABool =>
            &&(===(ao1, as2.first.asInstanceOf[AOption[AInt]]), ===(bool, as2.second.asInstanceOf[ABool]))
          case ao11: AOption[AInt] =>
            &&(===(ao1, as2.first.asInstanceOf[AOption[AInt]]), ===(ao11, as2.second.asInstanceOf[AOption[AInt]]))
          case a1: AOption[AList] =>
            &&(===(ao1, as2.first.asInstanceOf[AOption[AInt]]), ===(a1, as2.second.asInstanceOf[AOption[AList]]))
          case _ =>
            AFalse //if it is a different combination
        }
      case a1: AOption[AList] =>
        as1.second match {
          case int: AInt =>
            &&(===(a1, as2.first.asInstanceOf[AOption[AList]]), ===(int, as2.second.asInstanceOf[AInt]))
          case list: AList =>
            &&(===(a1, as2.first.asInstanceOf[AOption[AList]]), ===(list, as2.second.asInstanceOf[AList]))
          case bool: ABool =>
            &&(===(a1, as2.first.asInstanceOf[AOption[AList]]), ===(bool, as2.second.asInstanceOf[ABool]))
          case ao1: AOption[AInt] =>
            &&(===(a1, as2.first.asInstanceOf[AOption[AList]]), ===(ao1, as2.second.asInstanceOf[AOption[AInt]]))
          case a11: AOption[AList] =>
            &&(===(a1, as2.first.asInstanceOf[AOption[AList]]), ===(a11, as2.second.asInstanceOf[AOption[AList]]))
          case _ =>
            AFalse //if it is a different combination
        }
      case _ =>
        AFalse //if it is a different combination
    }


  }


  //Method checks whether an AList value contains an element of AInt
  def aContains(al1: AList, elem: AInt) : ABool = al1 match {
    case ANil => AFalse
    case ACons(h,t) => if(intervals.contains_Interval(elem, h)) ATrue else aContains(t, elem)
    case AMany(e) => if(intervals.contains_Interval(elem, e)) ATrue else AFalse
  }


  /************************************************************
   *                Operators returning Set[A]                *
   ************************************************************/
  //trait "separates" the input of a unary operations in two sets
  trait AUnOp[A]{
    def positive(a:A): Set[A]
    def negative(a:A): Set[A]
  }

  //checks whether an AList is empty and returns the empty and non-empty parts of it
  object AIsNil extends AUnOp[AList]{
    override def positive(a: AList): Set[AList] = a match{
      case ANil => Set(a)
      case ACons(_,_) => Set()
      case AMany(e) => Set(ANil)
    }

    override def negative(a: AList): Set[AList] = a match {
      case ANil => Set()
      case ACons(_,_) => Set(a)
      case AMany(e) => Set(ACons(e, AMany(e)))
    }
  }


  //trait "separates" inputs of unary operations in two sets
  trait ABinOp[A]{
    def positive(a1:A,a2: A ) :  Set[AInt]
    def negative(a1:A,a2: A) : (Set[(A,A)] , Set[(A,A)])
  }

    object AIntEqual extends ABinOp[AInt] {
      //returns the interval both inputs have in common
      override def positive(a1: AInt, a2: AInt): Set[AInt] = {
        if (intervals.intersect_Interval(a1,a2) != intervals.Interval(IntegerInf, IntegerNegInf)) Set(intervals.intersect_Interval(a1,a2)) else Set()
      }

      //returns the interval parts that are not equal: (a1(before, after), a2(before, after))
      override def negative(a1: AInt, a2: AInt): (Set[(AInt, AInt)], Set[(AInt, AInt)]) = {
       if  (AIntEqual.positive(a1,a2).isEmpty){ //not equal at all
         (Set((a1, null)), Set((a2, null)))
       }
        else{ //there are some parts of the intervals that are equivalent
           if(a1.lb == a2.lb){  //exactly the same boundaries
             if(a1.ub == a2.ub){
              (Set((null, null)), Set((null,null)))
             }else if(IntegerW.<(a1.ub, a2.ub)){
                 (Set((null, null)), Set((null,Interval(IntegerW.+(a1.ub, IntegerVal(1)), a2.ub))))
             }else{ //if(IntegerW.<(a2.ub, a1.ub))
                (Set((null, Interval(IntegerW.+(a2.ub, IntegerVal(1)), a1.ub))), Set((null, null)))
             }
           }else if(IntegerW.<(a1.lb, a2.lb)){
             if(a1.ub == a2.ub){
               (Set((Interval(a1.lb,IntegerW.-(a2.lb, IntegerVal(1))), null)), Set((null, null)))
             }else if(IntegerW.<(a1.ub, a2.ub)){
               (Set((Interval(a1.lb,IntegerW.-(a2.lb, IntegerVal(1))), null)), Set((null, Interval(IntegerW.+(a1.ub, IntegerVal(1)), a2.ub))))
             }else { //if(IntegerW.<(a2.ub, a1.ub))
               (Set((Interval(a1.lb, IntegerW.-(a2.lb, IntegerVal(1))),Interval(IntegerW.+(a2.ub, IntegerVal(1)),a1.ub))), Set((null, null)))
             }
           }else { //if(IntegerW.<(a2.lb, a1.lb))
             if(a1.ub == a2.ub){
               (Set((null,null)), Set((Interval(a2.lb, IntegerW.-(a1.lb, IntegerVal(1))),null)))
             }else if(IntegerW.<(a1.ub, a2.ub)){
               (Set((null, null)), Set((Interval(a2.lb, IntegerW.-(a1.lb, IntegerVal(1))),Interval(IntegerW.+(a1.ub, IntegerVal(1)), a2.ub))))
             }else { //if(IntegerW.<(a2.ub, a1.ub))
               (Set((null, Interval(IntegerW.+(a2.ub, IntegerVal(1)),a1.ub))), Set((Interval(a2.lb, IntegerW.-(a1.lb, IntegerVal(1))),null)))
             }
           }
       }
      }
    }

/*
  //TODO needs improvement
  object AListEqual extends ABinOp[AList] {
    override def positive(a1: AList, a2: AList): Set[AList] = (a1,a2) match { //return the equal part -> one AList
      case (ANil, ANil) => Set(ANil)
      case (ANil, AMany(_)) | (AMany(_), ANil)=> Set(ANil)
      case (ANil, ACons(_,_)) | (ACons(_,_), ANil) => Set()
      case (ACons(h1,t1), ACons(h2,t2)) => if(AIntEqual.positive(h1,h2).nonEmpty) Set(ACons(AIntEqual.positive(h1,h2).head, AListEqual.positive(t1,t2).head)) else Set()
      case (ACons(h,t), AMany(e)) =>  if(AIntEqual.positive(h,e).nonEmpty) Set(ACons(AIntEqual.positive(h,e).head, AListEqual.positive(t,a2).head)) else Set()
      case (AMany(e), ACons(h,t)) => if(AIntEqual.positive(e,h).nonEmpty) Set(ACons(AIntEqual.positive(e,h).head, AListEqual.positive(a1,t).head)) else Set()
      case (AMany(e1), AMany(e2)) => if(AIntEqual.positive(e1,e2).nonEmpty) Set(AMany(AIntEqual.positive(e1,e2).head)) else Set()
    }


    override def negative(a1: AList, a2: AList): (Set[(AList,AList)], Set[(AList,AList)]) =(a1,a2) match {
      case (ANil, ANil) => (Set(), Set())
      case (ANil, AMany(_)) => ???
      case(AMany(_), ANil) => ???
      case (ANil, ACons(_,_)) => ???
      case (ACons(_,_), ANil) => ???
      case (ACons(h1,t1), ACons(h2,t2)) => ???
      case (ACons(h,t), AMany(e)) =>  ???
      case (AMany(e), ACons(h,t)) => ???
      case (AMany(e1), AMany(e2)) => ???
    }

*/




  /************************************************************
   *             Methods:   AOption[T] -> T                  *
   ************************************************************/

  //Method returns the argument of an AOption value
  def justValue[T](ao: AOption[T]): Set[AState_Base] = ao match {
    case ASome(e) =>  Set(AState(ASome(e).get, ATrue))
    case ANone => Set(AState(ANone, AFalse))// throw new Exception("Exception thrown from justAList. Reason: Input was ANone")
    case AMaybe(e) => Set(AState(ANone, AFalse), AState(ASome(e).get, ATrue))
      // throw new Exception("Exception thrown from justAList. Reason: Input was AMaybe")
  }


  /************************************************************
   *                       Lattice                            *
   ************************************************************/

  implicit def Lattice: Lattice[AList] = new Lattice[AList] {

    override def bot: AList = ACons(intervals.Lattice.bot, ANil)

    override def top: AList = AMany(intervals.Lattice.top)

    override def lub(a1: AList, a2: AList): AList = intersect_AList(a1: AList, a2: AList)

    override def glb(a1: AList, a2: AList): AList = union_AList(a1: AList, a2: AList)

    override def <=(a1: AList, a2: AList): Boolean = subset_AList(a1: AList, a2: AList)

    override def widen(a1: AList, a2: AList, bound: Int): AList = widen_AList(a1: AList, a2: AList)
  }

  /************************************************************
   *                      AState, AStmt                       *
   ************************************************************/

  trait AState_Base{
    def  first: Any
    def  second: Any
  }

  //Represents an abstract state value. Is used by objects of AStmt and in Sets for Sequences
  case class AState(first:Any, second: Any) extends AState_Base


  //Representation of a Statement. Is used by Sequences, e.g. IfElse_xsIsNil(stmt1,stmt2)
  trait AStmt {
    def execute(as: Set[AState_Base]): Set[AState_Base]
  }

  //Statements assigns interval [0;0] to a Set of AStates -> initialState
  object AssignN0 extends AStmt { //initial, beginning of loop
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        if(a.first.isInstanceOf[AInt]) {
          result += AState(Interval(IntegerVal(0), IntegerVal(0)), a.second)
        } else{
          result += a
        }
      }
      result
    }
  }

  //Statements assigns a interval [1;1] to a Set of AStates -> initialState
  object AssignN1 extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        if(a.first.isInstanceOf[AInt] ) {
          result += AState(Interval(IntegerVal(1), IntegerVal(1)), a.second)
        } else{
          result += a
        }
      }
      result
    }
  }


  //Statements assigns AFalse to a Set of AStates -> initialState
  object AssignAFalse extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        if(a.first.isInstanceOf[ABool] ) {
          result += AState(AFalse, a.second)
        } else{
          result += a
        }
      }
      result
    }
  }


  //Statements assigns ATrue to a Set of AStates -> initialState
  object AssignATrue extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        if(a.first.isInstanceOf[ABool] ) {
          result += AState(ATrue, a.second)
        } else{
          result += a
        }
      }
      result
    }
  }


  //Statements adds interval [1;1] to a Set of AStates
  object Add1 extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {

        a.first match {
          case int: AInt =>
            result += AState(intervals.+(int, Interval(IntegerVal(1), IntegerVal(1))), a.second)
          case _ =>
            result += a
        }

      }
      result
    }
  }

  //Statements subtracts a interval [1;1] off a Set of AStates
  object Subtract1 extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        a.first match {
          case int: AInt =>
            result += AState(intervals.-(int, Interval(IntegerVal(1), IntegerVal(1))), a.second)
          case _ =>
            result += a
        }
      }
      result
    }
  }

  object Subtract1_ATail extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        a.first match {
          case int: AInt if a.second.isInstanceOf[AList] =>
            result += AState(intervals.-(int, Interval(IntegerVal(1), IntegerVal(1))), justValue(aTail(a.second.asInstanceOf[AList])).head)
          case _ =>
            result += a
        }
      }
      result
    }
  }

  object Add1_ATail extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        a.first match {
          case int: AInt if a.second.isInstanceOf[AList] =>
            result += AState(intervals.+(int, Interval(IntegerVal(1), IntegerVal(1))), justValue(aTail(a.second.asInstanceOf[AList])).head)
          case _ =>
            result += a
        }
      }
      result
    }
  }

  object Assign_SameValues extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (a <- as) {
        result += a
      }
      result
    }
  }




  /*
  /** object of AStmt.
   * Method execute assigns interval [0;0] to a Set of AStates -> initialState
   * Method assignAnyN assigns any interval n to a Set of AStates
   */
  //TODO useful approach?
  object AssignN extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(Interval(IntegerVal(0), IntegerVal(0)), a.xs)
      }
      result
    }

    def assignAnyN(n: AInt, as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(n, a.xs)
      }
      result
    }
  }

  /** object of AStmt.
   * Method execute subtracts the interval [1;1] to a Set of AStates
   * Method addAnyN subtracts any interval n off a Set of AStates
   */
  object SubtractAnyN extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.-(a.n, Interval(IntegerVal(1), IntegerVal(1))), a.xs)
      }
      result
    }

    def subtractAnyN(n: AInt, as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.-(a.n, n), a.xs)
      }
      result
    }

    def subtractAnyN_aTail(n: AInt, as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.-(a.n, n), justValue(aTail(a.xs)).head)
      }
      result
    }
  }


  /** object of AStmt.
   * Method execute adds the interval [1;1] to a Set of AStates
   * Method addAnyN adds any interval n to a Set of AStates
   */
  object AddAnyN extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.+(a.n, Interval(IntegerVal(1), IntegerVal(1))), a.xs)
      }
      result
    }

    def addAnyN(n: AInt, as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.+(a.n, n), a.xs)
      }
      result
    }

    def addAnyN_aTail(n: AInt, as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.+(a.n, n), justValue(aTail(a.xs)).head)
      }
      result
    }
  }
*/

  /************************************************************
   *                     Sequences                            *
   ************************************************************/

  //Method is used to create an AState of a given list and an already existing AState -> IfElse_xsIsNil
  /* TODO needs improvement
  def AssignN_SameIntervalToAList(as: AState_Base, al: AList) : AState = {
    AState_AnyAny(as.first, al)
  }
   */

  //Method splits a given AList into two sets (Empty AList, Non-Empty AList)
  def ifIsNil(l: AList): (Set[AList], Set[AList]) = l match {
    case ANil => (Set(ANil), Set())
    case ACons(h, t) => (Set(), Set(ACons(h, t)))
    case AMany(e) => (Set(ANil), Set(ACons(e, AMany(e))))
  }

/* TODO deprecated
  //stmt1 will be executed if xs of the given AState is nil, otherwise stmt2 will be executed
  case class IfElse_xsIsNil(stmt1: AStmt, stmt2: AStmt) extends AStmt {
      def execute(as: Set[AState_Base]): Set[AState_Base] = {
        var result: Set[AState_Base] = Set()
        for (a <- as) {
          val (isNil, isNotNil) = ifIsNil(a.second.asInstanceOf[AList])

          val aStatesTrue: Set[AState] = isNil.map(AssignN_SameIntervalToAList(a,_))
          val aStatesFalse: Set[AState] = isNotNil.map(AssignN_SameIntervalToAList(a,_)) //isNotNil -> aStatesFalse

          result =  stmt1.execute(aStatesTrue) ++ stmt2.execute(aStatesFalse)
        }
        result
      }
    }



//The AStmt will be executed if xs of the given AState is nil
  //TODO needs improvement/Deprecated
  case class If_xsIsNil(stmt: AStmt) {
    def execute(as: AState_Base): Set[AState_Base] = {
      val (aStatesTrue, aStatesFalse) = ifIsNil(as.second.asInstanceOf[AList])
      val result = for (as1 <- aStatesTrue; as2 <- stmt.execute(Set(as))) yield as2
      result
    }
  }
  */


  /************************************************************
   *                          ATest                           *
   ************************************************************/

  //Trait ATest represents the two states a Test (e.g. xsIsNil) can have
  trait ATest{
    def positive(states: Set[AState_Base]): Set[AState_Base]
    def negative(states: Set[AState_Base]): Set[AState_Base]
  }

/*
  object xsIsNilTest extends ATest {
    override def positive(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {       //1. check if it is an AList
                                    //2. add this state to the result
        val (isNil, _) = ifIsNil(state.xs)
         result ++= isNil.map(AssignN_SameIntervalToAList(state,_))
      }
     result
    }

    override def negative(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {
        val (_, isNotNil) = ifIsNil(state.xs)
        result ++= isNotNil.map(AssignN_SameIntervalToAList(state,_))
      }
      result
    }
  }

*/
  /*
  object xsIsNotNilTest extends ATest {
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        val (_, isNotNil) = ifIsNil(state.xs)
        result = isNotNil.map(AssignN_SameIntervalToAList(state,_))
      }
      result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        val (isNil, _) = ifIsNil(state.xs)
        result = isNil.map(AssignN_SameIntervalToAList(state,_))
      }
      result
      }
  }
*/

  object nIsPositive extends ATest {
    override def positive(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if intervals.isPositive(int) =>
            result += state
          case _ =>
        }
      }
      result
    }

    override def negative(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if !intervals.isPositive(int) =>
            result += state
          case _ =>
        }
      }
      result
    }
  }


  object nIsNegative extends ATest {
    override def positive(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if intervals.isNegative(int) =>
            result += state
          case _ =>
        }
      }
      result
    }

    override def negative(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if !intervals.isNegative(int) =>
            result += state
          case _ =>
        }
      }
      result
    }
  }


  object nEqualsZero extends ATest {
    override def positive(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if intervals.isZero(int) =>
            result += state
          case _ =>
        }
      }
      result
    }

    override def negative(states: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if !intervals.isZero(int) =>
            result += state
          case _ =>
        }
      }
      result
    }
  }

  /************************************************************
   *                 Abstract Transformers                    *
   ************************************************************/

  //Abstract transformer: AIf is an abstract representation of an If test
  case class AIf(test: ATest, left: AStmt, right:AStmt) extends AStmt{
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      left.execute(test.positive(as)) ++ right.execute(test.negative(as))
    }
  }

  //Abstract transformer: AWhile is an abstract representation of a while loop
  case class AWhile(test:ATest, body: AStmt) extends AStmt {
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var states_at_loop_head: Set[AState_Base] = as
      var states_after_body: Set[AState_Base] = Set()
      var result: Set[AState_Base] = Set()
      var abort_loop = false

      while (test.positive(states_at_loop_head).nonEmpty && (!abort_loop)) {
        states_after_body = body.execute(test.positive(states_at_loop_head))
        println("After loop-execution:" + states_after_body.head)
       // result = Set(AState(intervals.Lattice.widen(states_at_loop_head.head.first, states_after_body.head.n), widen_AList(states_at_loop_head.head.xs, states_after_body.head.xs)))
        println("After widen:" + result.head)
        println("")

        //TODO TEST
        if (AStateEqual(states_at_loop_head.head, result.head) == AFalse)  {
          states_at_loop_head = result
        } else if (AStateEqual(states_at_loop_head.head, result.head) == ATrue) {
          abort_loop = true
        }


      }

      if (result.nonEmpty) {
        println("After loop:" + result)
      } else {
        println("Not in loop")
      }
      result
    }
  }


  //Abstract Transformer: AAssert is an abstract representation of an assertion
  //TODO perhaps: extends AStmt -> use output for AVerify()
  case class AAssert(test: ATest) {
     def execute(as: Set[AState_Base]) : Unit = {
       if (test.positive(as).nonEmpty) {
         Console.println(s"$RESET${GREEN}Assertion fulfills for state(s): $RESET")  //success
         test.positive(as).foreach(posElement => Console.println(s"$RESET$GREEN $posElement $RESET"))
       }
       println("")

       if (test.negative(as).nonEmpty) {
         Console.println(s"$RESET${RED}Assertion fails for state(s):$RESET") //fails
         test.negative(as).foreach(negElement => Console.println(s"$RESET$RED $negElement $RESET"))
         //-> return input
       }

       //yellow for might fail/unknown/warning -> return input
    }
  }

  //TODO outer verification of the program code
  //case class AVerify()

  //Abstract Transformer: AAssume is an abstract representation of an assumption
  //TODO Testcases + scenarios -> functionality
  case class AAssume(assumption: AStmt) extends AStmt{
    override def execute(as: Set[AState_Base]): Set[AState_Base] = {
      var result: Set[AState_Base] = assumption.execute(as)
      result
    }
  }




  /************************************************************
   *                     Concretization                       *
   ************************************************************/

  //Method concretizes the abstract type ABool into concrete values of Boolean
  def concretization_ABool(ab: Set[ABool]): Set[Boolean] = ab.map {
    case ATrue => true
    case AFalse => false
  }



}










