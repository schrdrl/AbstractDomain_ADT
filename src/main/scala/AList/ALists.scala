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

  sealed trait AList //"behaviour"
  case object ANil extends AList
  case class ACons(head: AInt, tail: AList) extends AList
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
   *                Operators (&&, ===, !==)                  *
   ************************************************************/

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
    if (a.equals(b)) {
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
    case (AMany(a), ACons(b, bs)) => AFalse
    case (ACons(a, as), AMany(b)) => AFalse
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

  def ===(as1: AState, as2: AState): ABool = {
    &&(===(as1.n, as2.n),===(as1.xs, as2.xs))
  }


  /************************************************************
   *             Methods:   AOption[T] -> T                  *
   ************************************************************/


  //Pattern Matching is done with case class Sequence (e.g. IfElse_xsIsNil)
  def justAList(ao: AOption[AList]): AList = ao match {
    case ASome(e) => e
    case ANone => throw new Exception("Exception thrown from justAList. Reason: Input was ANone")
    case AMaybe(_) => throw new Exception("Exception thrown from justAList. Reason: Input was AMaybe")
  }

  def justAInt(ao: AOption[AInt]): AInt = ao match{
    case ASome(e) => e
    case ANone => throw new Exception("Exception thrown from justAInt. Reason: Input was ANone")
    case AMaybe(_) => throw new Exception("Exception thrown from justAInt. Reason: Input was AMaybe")
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
  //Represents a state(AInt, AList). Is used by objects of AStmt and in Sets for Sequences
  case class AState(n: AInt, xs: AList)

  //Representation of a Statement. Is used by Sequences, e.g. IfElse_xsIsNil(stmt1,stmt2)
  trait AStmt {
    def execute(as: Set[AState]): Set[AState]
  }

  //Statements assigns interval [0;0] to a Set of AStates -> initialState
  object AssignN0 extends AStmt { //initial, beginning of loop
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(Interval(IntegerVal(0), IntegerVal(0)), a.xs)
      }
      result
    }
  }

  //Statements assigns a interval [1;1] to a Set of AStates -> initialState
  object AssignN1 extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(Interval(IntegerVal(1), IntegerVal(1)), a.xs)
      }
      result
    }
  }

  //Statements adds interval [1;1] to a Set of AStates
  object Add1 extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.+(a.n, Interval(IntegerVal(1), IntegerVal(1))), a.xs)
      }
      result
    }
  }

  //Statements subtracts a interval [1;1] off a Set of AStates
  object Subtract1 extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.-(a.n, Interval(IntegerVal(1), IntegerVal(1))), a.xs)
      }
      result
    }
  }

  object Subtract1_ATail extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.-(a.n, Interval(IntegerVal(1), IntegerVal(1))), justAList(aTail(a.xs)))
      }
      result
    }
  }

  object Add1_ATail extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(intervals.+(a.n, Interval(IntegerVal(1), IntegerVal(1))), justAList(aTail(a.xs)))
      }
      result
    }
  }

  object Assign_SameValues extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += AState(a.n, a.xs)
      }
      result
    }
  }

  /** object of AStmt.
   * Method execute assigns interval [0;0] to a Set of AStates -> initialState
   * Method assignAnyN assigns any interval n to a Set of AStates
   */
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
        result += AState(intervals.-(a.n, n), justAList(aTail(a.xs)))
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
        result += AState(intervals.+(a.n, n), justAList(aTail(a.xs)))
      }
      result
    }
  }


  /************************************************************
   *                     Sequences                            *
   ************************************************************/

  //Method is used to create an AState of a given list and an already existing AState -> IfElse_xsIsNil
  def AssignN_SameIntervalToAList(as: AState, al: AList) : AState = {
    AState(as.n, al)
  }

  //Method splits a given AList into two sets (Empty AList, Non-Empty AList)
  def ifIsNil(l: AList): (Set[AList], Set[AList]) = l match {
    case ANil => (Set(ANil), Set())
    case ACons(h, t) => (Set(), Set(ACons(h, t)))
    case AMany(e) => (Set(ANil), Set(ACons(e, AMany(e))))
  }



  //stmt1 will be executed if xs of the given AState is nil, otherwise stmt2 will be executed
  case class IfElse_xsIsNil(stmt1: AStmt, stmt2: AStmt) extends AStmt {
      def execute(as: Set[AState]): Set[AState] = {
        var result: Set[AState] = Set()
        for (a <- as) {
          val (isNil, isNotNil) = ifIsNil(a.xs)

          val aStatesTrue: Set[AState] = isNil.map(AssignN_SameIntervalToAList(a,_))
          val aStatesFalse: Set[AState] = isNotNil.map(AssignN_SameIntervalToAList(a,_)) //isNotNil -> aStatesFalse

          result =  stmt1.execute(aStatesTrue) ++ stmt2.execute(aStatesFalse)
        }
        result
      }
    }



//The AStmt will be executed if xs of the given AState is nil
  case class If_xsIsNil(stmt: AStmt) {
    def execute(as: AState): Set[AState] = {
      val (aStatesTrue, aStatesFalse) = ifIsNil(as.xs)
      val result = for (as1 <- aStatesTrue; as2 <- stmt.execute(Set(as))) yield as2
      result
    }
  }

  /************************************************************
   *                          ATest                           *
   ************************************************************/

  //Trait ATest represents the two states a Test (e.g. xsIsNil) can have
  trait ATest{
    def positive(states: Set[AState]): Set[AState]
    def negative(states: Set[AState]): Set[AState]
  }


  object xsIsNilTest extends ATest {
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        val (isNil, _) = ifIsNil(state.xs)
         result = isNil.map(AssignN_SameIntervalToAList(state,_))
      }
     result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        val (_, isNotNil) = ifIsNil(state.xs)
        result = isNotNil.map(AssignN_SameIntervalToAList(state,_))
      }
      result
    }
  }

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


  object nIsPositive extends ATest {
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        if(intervals.isPositive(state.n)){
          result += state
        }
      }
      result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        if(!intervals.isPositive(state.n)){
          result += state
        }
      }
      result
    }
  }


  object nIsNegative extends ATest {
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        if(intervals.isNegative(state.n)){
          result += state
        }
      }
      result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        if(!intervals.isNegative(state.n)){
          result += state
        }
      }
      result
    }
  }


  object nEqualsZero extends ATest {
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        if(intervals.isZero(state.n)){
          result += state
        }
      }
      result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        if(!intervals.isZero(state.n)){
          result += state
        }
      }
      result
    }
  }

  case class AIf(test: ATest, left: AStmt, right:AStmt) extends AStmt{
    override def execute(as: Set[AState]): Set[AState] = {
      left.execute(test.positive(as)) ++ right.execute(test.negative(as))
    }
  }


  case class AWhile(test:ATest, body: AStmt) extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var states_at_loop_head: Set[AState] = as
      var states_after_body: Set[AState] = Set()
      var result: Set[AState] = Set()
      var abort_loop = false

      while (test.positive(states_at_loop_head).nonEmpty && (!abort_loop)) {
        states_after_body = body.execute(test.positive(states_at_loop_head))
        println("After loop-execution:" + states_after_body.head)
        result = Set(AState(intervals.Lattice.widen(states_at_loop_head.head.n, states_after_body.head.n), widen_AList(states_at_loop_head.head.xs, states_after_body.head.xs)))
        println("After widen:" + result.head)
        println("")

        if (===(states_at_loop_head.head, result.head) == AFalse)  {
          states_at_loop_head = result
        } else if (===(states_at_loop_head.head, result.head) == ATrue) {
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



  //Assert all possible states
  case class AAssert(test: ATest) {
     def execute(as: Set[AState]) : Unit = {
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

       //yellow for might fail/unknown -> return input

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










