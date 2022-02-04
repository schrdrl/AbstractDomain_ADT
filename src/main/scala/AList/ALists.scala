package AList

import Expressions.ABinOp

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

  //Method splits a given AList into two sets (Empty AList, Non-Empty AList)
  def ifIsNil(l: AList): (Set[AList], Set[AList]) = l match {
    case ANil => (Set(ANil), Set())
    case ACons(h, t) => (Set(), Set(ACons(h, t)))
    case AMany(e) => (Set(ANil), Set(ACons(e, AMany(e))))
  }

  //Method sorts a given AList value into a Set of AStates
  def isNil_AState(l: AList): Set[AState]= l match {
    case ANil => Set(AState(Map(("AList0", ANil))))
    case ACons(h, t) => Set(AState(Map(("AList1", ACons(h, t)))))
    case AMany(e) => Set(AState(Map(("AList0", ANil), ("AList1", ACons(e, AMany(e))))))
  }

  /************************************************************
   *                   isConcreteElementOf                    *
   ************************************************************/

  //checks whether an integer value is a concrete Element of an Interval.
  def isConcreteElementOf_AInt(i: Int, ai: AInt): Boolean = {
    intervals.contains(ai, i)
  }

  //checks whether a list is a concrete Element of AList.
  def isConcreteElementOf_AList(l: List[Int], al: AList): Boolean = (l, al) match {
    case (Nil, ANil) => true
    case (Nil, ACons(_, _)) => false
    case (Nil, AMany(_)) => true //AMany = ANil ≀ ACons(e, Many(e))
    case (x :: xs, ANil) => false
    case (x :: xs, ACons(ax, axs)) => isConcreteElementOf_AInt(x, ax) && isConcreteElementOf_AList(xs, axs)
    case (x :: xs, AMany(ax)) =>
      isConcreteElementOf_AInt(x, ax) && isConcreteElementOf_AList(xs, al)
  }

  //checks whether an Option[Int] is a concrete Element of an interval AOption[AInt].
  def isConcreteElementOf_AOptionAInt(o: Option[Int], ao: AOption[AInt]): Boolean = (o, ao) match {
    case (None, ANone) => true
    case (None, ASome(_)) => false
    case (None, AMaybe(_)) => true
    case (Some(_), ANone) => false
    case (Some(h1), ASome(h2)) => intervals.contains(h2, h1)
    case (Some(h1), AMaybe(h2)) => intervals.contains(h2, h1)
  }

  //checks whether an Option[List[Int]] is a concrete Element of an interval AOption[AList].
  def isConcreteElementOf_AOptionAList(o: Option[List[Int]], ao: AOption[AList]): Boolean = (o, ao) match {
    case (None, ANone) => true
    case (None, ASome(_)) => false
    case (None, AMaybe(_)) => true
    case (Some(_), ANone) => false
    case (Some(h1), ASome(h2)) => isConcreteElementOf_AList(h1, h2)
    case (Some(h1), AMaybe(h2)) => isConcreteElementOf_AList(h1, h2)
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

   //Method prepends an element on the front a an AList value
   def +:(elem: AInt, al2: AList) : AList = ACons(elem, al2 )

  //TODO recheck
  //Method appends an element the the end a an AList value
   def :+(al1: AList, elem: AInt) : AList = al1 match {
     case ANil => ACons(elem, ANil)
     case ACons(h,t) => ACons(h,:+(t, elem))
     //if(:+(t, elem).tail.nonEmpty) Set(ACons(h, :+(t, elem).head), ACons(h, :+(t, elem).tail.head)) else Set(ACons(h, :+(t, elem).head))
     case AMany(e) => AMany(intervals.Lattice.widen(e, elem)) //TODO test
    //Set(ACons(elem, ANil), ACons(e, AMany(intervals.union_Interval(e, elem))))
     //1. case: ANil -> ACons(elem, ANil)
     //2. case: ACons(e, AMany(e)) -> ACons(e, ACons(elem, ANil)) oder ACons(e, ACons(elem, AMany(e))) oder ACons(e, AMany(e union elem)) ???
   }

  //TODO recheck
  //Method concatenates two values of type AList
  //TODO is union the best solution here? -> interval can be too wide
   def ++(al1: AList, al2: AList): AList = (al1, al2) match { //TODO : AList
     case (ANil, ANil) =>ANil
     case (ANil, ACons(h,t)) => ACons(h,t)
     case (ACons(h,t), ANil) => ACons(h,t)
     case (ANil, AMany(e)) => AMany(e)
     case (AMany(e), ANil) => AMany(e)
     case (AMany(e1), AMany(e2)) => AMany(intervals.union_Interval(e1,e2))
     case (AMany(e1), ACons(h,t)) => union_AList(al1, flatten_AList(al2))//Set(al2, ACons(e1, union_AList(al1, al2))) //TODO recheck
     case (ACons(h,t), AMany(e2)) => ACons(h, ++(t, al2))
     //if(++(t,ACons(e2,AMany(e2))).tail.nonEmpty) Set(ACons(h, ++(t,ACons(e2,AMany(e2))).head), ACons(h, ++(t,ACons(e2,AMany(e2))).tail.head)) else Set(al1, ACons(h,++(t,ACons(e2,AMany(e2))).head)) //TODO recheck
     case (ACons(h1, t1), ACons(h2, t2)) => ACons(h1, ++(t1, al2))
    //if(++(t1, al2).tail.nonEmpty) Set(ACons(h1, ++(t1, al2).head),ACons(h1, ++(t1, al2).tail.head)) else Set(ACons(h1, ++(t1, al2).head))
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

  //TODO recheck -> widen or union_AList
  //Method flattens a given AList value (not empty) to a more compact AMany-format
  def flatten_AList(al: AList) : AList = al match {
    case ANil => ANil
    case ACons(h,t) => union_AList(AMany(h), t)
    case AMany(e) => AMany(e)
  }

  //TODO check reverse
  /**
   * Input: xs Local: ys = [] -> empty List
   * while xs != [] do
   * ys = Cons(xs.head, ys) xs = xs.tail
   */
  def reverse(al: AList) : AList = al match {
    case ANil => ANil
    case ACons(h,t) => {
      var axs : AList = al
      var ays : AList = ANil


      //TODO needs improvement
      while(===(axs, ANil) == AFalse){  //TODO ABool ===
       // if(===(===[AList](t, AMany(_)), ATrue) == ATrue){
         //TODO ACons -> tail starts with AMany: ays = AMany}
        /*
        ays = ACons(justValue(aHead(axs)).head.first.asInstanceOf[AInt], ays) //TODO justValue
        axs = justValue(aTail(axs)).head.first.asInstanceOf[AList]  //TODO justValue

         */
      }
      ays
    }
    //ACons([1,1], ANil) -> ACons([1,1], ANil)
    //ACons([1,1], ACons([2,2], ACons([3,3], ANil))) -> ACons([3,3], ACons([2,2], ACons([1,1], ANil)))
    //ACons([1,1], AMany([2,2])) ->AMany([1,2])
    case AMany(e) => AMany(e)

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


  //Method checks whether an AList value contains an element of AInt
  def aContains(al1: AList, elem: AInt) : ABool = al1 match {
    case ANil => AFalse
    case ACons(h,t) => if(intervals.contains_Interval(elem, h)) ATrue else aContains(t, elem)
    case AMany(e) => if(intervals.contains_Interval(elem, e)) ATrue else AFalse
  }



  /************************************************************
   *             Methods:   AOption[T] -> T                  *
   ************************************************************/

  //Method returns the argument of an AOption value
  def justValue[T](ao: AOption[T]): Set[AState] = ao match {
    case ASome(e) =>  Set(AState(Map(("ASome", ASome(e).get),("ASome", ATrue))))
    case ANone => Set(AState(Map(("ASome", AFalse))))   // throw new Exception("Exception thrown from justAList. Reason: Input was ANone")
    case AMaybe(e) => Set(AState(Map(("ASome", AFalse))), AState(Map(("ASome", ASome(e).get),("ASome", ATrue))))  // throw new Exception("Exception thrown from justAList. Reason: Input was AMaybe")
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

  //Abstract representation on an expression
  trait AExpr {
    def evaluate(as: AState) : Any
    def +(that: AExpr): ABinOp = ABinOp(this, "+", that) //nested expression
  }

  //Abstract representation of a state
  case class AState(values: Map[String, Any]) {
    def lookup(name: String): Any = values(name)
    def updated(name: String, value: Any) : AState = AState(values+(name -> value))
  }

  //Assigns a new AExpr to an AState value
  case class AAssign(name: String, expr: AExpr)  {
    def execute(as: Set[AState]): Set[AState] = {
      for(a <- as) yield {
        val value = expr.evaluate(a)
        a.updated(name, value)
      }
    }
  }


  //Abstract representation of a constant
  case class AConst(value:Any) extends AExpr {
    override def evaluate(as: AState): Any = value
  }

  //Binary operators
  case class ABinOp(left:AExpr, op: String, right: AExpr) extends AExpr {
    override def evaluate(as: AState): Any = {
      val l = left.evaluate(as)
      val r = right.evaluate(as)
      (l,op,r) match {
        case (l: Int,"+" ,r: Int) => l+r
        case (l: AInt,"+" ,r: AInt) => intervals.+(l,r)
        case (l: AInt,"-" ,r: AInt) => intervals.-(l,r)
        case(l: AList, "union", r: AList) => union_AList(l,r)
        case(l: AInt, "union", r: AInt) => intervals.union_Interval(l,r)
        case(l: AOption[AInt], "union", r: AOption[AInt]) => union_AOption_AInt(l,r)
        case(l: AOption[AList], "union", r: AOption[AList]) => union_AOption_AList(l,r)
        case(l: ABool, "union", r: ABool) => union_ABool(l,r)
        case(l: AList, "intersect", r: AList) => intersect_AList(l,r)
        case(l: AInt, "intersect", r: AInt) => intervals.intersect_Interval(l,r)
        case(l: AOption[AInt], "intersect", r: AOption[AInt]) => intersect_AOption_AInt(l,r)
        case(l: AOption[AList], "intersect", r: AOption[AList]) => intersect_AOption_AList(l,r)
        case(l: ABool, "intersect", r: ABool) => intersect_ABool(l,r)
        case(l: AList, "subset", r: AList) => subset_AList(l,r)
        case(l: AList, "widen", r: AList) => widen_AList(l,r)
        //TODO
      }

    }
  }

  //Unary operators
  case class AUnOp(op: String, aexpr: AExpr) extends AExpr {
    override def evaluate(as: AState): Any = {
      val ae = aexpr.evaluate(as)
      (op,ae) match {
        case ("aHead" ,ae: AList) => aHead(ae)
        case ("aTail", ae: AList) => aTail(ae)
        case ("aLength", ae: AList) => aLength(ae)
        //TODO
      }

    }
  }






  //Representation of a Statement. Is used by Sequences, e.g. IfElse_xsIsNil(stmt1,stmt2)
  //TODO still necessary? Or just use AExpr
  trait AStmt{
    def execute(as: Set[AState]): Set[AState]
  }



  /************************************************************
   *                          ATest                           *
   ************************************************************/

  //Trait ATest represents the two states a test has as output
  trait ATest{
    def positive(states: Set[AState]): Set[AState]
    def negative(states: Set[AState]): Set[AState]
  }




  /************************************************************
   *                 Abstract Transformers                    *
   ************************************************************/

  //Abstract transformer: AIf is an abstract representation of an If test
  case class AIf(test: ATest, left: AStmt, right:AStmt) extends AStmt{
    override def execute(as: Set[AState]): Set[AState] = {
      left.execute(test.positive(as)) ++ right.execute(test.negative(as))
    }
  }

  //TODO AIF and AIF_ELSE




  //Abstract transformer: AWhile is an abstract representation of a while loop
  //TODO needs improvement
  case class AWhile(test:ATest, body: AStmt) extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var states_at_loop_head: Set[AState] = as
      var states_after_body: Set[AState] = Set()
      var result: Set[AState] = Set()
      var abort_loop = false

      while (test.positive(states_at_loop_head).nonEmpty && (!abort_loop)) {
        states_after_body = body.execute(test.positive(states_at_loop_head))
        println("After loop-execution:" + states_after_body.head)
       // result = Set(AState(intervals.Lattice.widen(states_at_loop_head.head.first, states_after_body.head.n), widen_AList(states_at_loop_head.head.xs, states_after_body.head.xs)))
        println("After widen:" + result.head)
        println("")

        //TODO TEST
        /*
        if (AStateEqual(states_at_loop_head.head, result.head) == AFalse)  {
          states_at_loop_head = result
        } else if (AStateEqual(states_at_loop_head.head, result.head) == ATrue) {
          abort_loop = true
        }


         */

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
  //TODO perhaps: extends AStmt -> use output positive/negative for AVerify()
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

       //yellow for might fail/unknown/warning -> return input
    }
  }

  //TODO outer verification of the program code
  //case class AVerify()

  //Abstract Transformer: AAssume is an abstract representation of an assumption
  //TODO Testcases + scenarios -> functionality
  case class AAssume(assumption: AStmt) extends AStmt{
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = assumption.execute(as)
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










