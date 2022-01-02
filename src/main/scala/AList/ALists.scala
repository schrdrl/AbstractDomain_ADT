package AList

/**
 * AList is an abstract domain of numerical lists (belonging to algebraic data types)
 * It is described by an interval and has three types:
 * -ANil: describes the empty AList
 * -ACons(h,t): has a specified head with associated interval and a tail of type AList
 * -AMany(e): contains both types ANil and ACons
 */
case class ALists(intervals: Intervals){
  import intervals.Interval //import inner Class
  type AInt = Interval  //alias

  sealed trait AList  //"behaviour"
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
  case object AUnknown extends ABool


/*Method returns the head of aList object, which is of type AOption[AInt]
*/
  def aHead (l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(h, _) => ASome(h)
    case AMany(e) => AMaybe(e) //AMany = ANil ≀ ACons(e, Many(e))
  }

  /*Method returns the tail of aList object, which is of type AOption[AInt]
*/
  def aTail(l: AList): AOption[AList] = l match {
    case ANil => ANone
    case ACons(_,t) => ASome(t)
    case AMany(e) => AMaybe(l) //l instead of AMany(e)
  }

  //Method returns the length of aList object, which is of type AOption[AInt]

  def aLength(l: AList): AOption[AInt] = l match {
    case ANil => ANone
    case ACons(_, _) => ASome(Interval(IntegerVal(1), IntegerInf))
    case AMany(_) => ASome(Interval(IntegerVal(0), IntegerInf))
  }

  // Method checks whether a given AList is Nil
  def isNil(l: AList): ABool = l match {
    case ANil => ATrue
    case ACons(_,_) => AFalse
    case AMany(_) => AUnknown
  }


  /*Method checks whether an integer value is a concrete Element of an Interval.
   *It takes two parameters, an integer and an interval of type AInt and returns a boolean
   */
  def isConcreteElementOf_Int(i: Int, ai: AInt): Boolean ={
    intervals.contains(ai, i)
  }

  /*Method checks whether a list is a concrete Element of AList.
   *It takes two parameters, a list with integer values and a abstract AList and returns a boolean
   */
  def isConcreteElementOf_List(l: List[Int], al:AList): Boolean = (l, al) match{
    case (Nil, ANil) => true
    case (Nil, ACons(_,_)) => false
    case (Nil, AMany(_)) => true //AMany = ANil ≀ ACons(e, Many(e))
    case (x::xs, ANil) => false
    case (x::xs, ACons(ax, axs)) => isConcreteElementOf_Int(x, ax) && isConcreteElementOf_List(xs, axs)
    case (x::xs, AMany(ax)) =>
      isConcreteElementOf_Int(x, ax) && isConcreteElementOf_List(xs, al)
  }


  /*Method checks whether an Option[Int] is a concrete Element of an interval AOption[AInt].
   *It takes two parameters, an integer and an interval of type AInt and returns a boolean
   */
  def isConcreteElementOf_Option(o: Option[Int], ao: AOption[AInt]): Boolean = (o,ao) match {
    case (None, ANone) => true
    case (None, ASome(_)) => false
    case (None, AMaybe(_)) => true
    case (Some(_), ANone) => false
    case (Some(h1), ASome(h2)) => intervals.contains(h2, h1)
    case (Some(h1), AMaybe(h2)) => intervals.contains(h2, h1)
  }

  def union_AList(al1: AList, al2: AList) : AList = (al1, al2) match {
    case (ANil, ANil) => ANil
    case (ANil, AMany(e)) => AMany(e)
    case (AMany(e), ANil) => AMany(e)
    case (ANil, ACons(a,as)) => union_AList(AMany(a), as)
    case (ACons(a,as), ANil) => union_AList(AMany(a), as)
    case (AMany(a), AMany(b)) => AMany(intervals.union_Interval(a,b))
    case (ACons(a,as), AMany(e)) => union_AList(AMany(intervals.union_Interval(a,e)), as)
    case (AMany(e), ACons(a,as)) => union_AList(AMany(intervals.union_Interval(a,e)), as)
    case (ACons(a,as), ACons(b, bs)) => ACons(intervals.union_Interval(a,b), union_AList(as,bs))
  }

  def union_AOption_AInt(ao1: AOption[AInt], ao2: AOption[AInt]) : AOption[AInt] = (ao1,ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, AMaybe(e)) => AMaybe(e)
    case (AMaybe(e), ANone) => AMaybe(e)
    case (ANone, ASome(e)) => AMaybe(e)
    case (ASome(e), ANone) => AMaybe(e)
    case (ASome(a), ASome(b)) => ASome(intervals.union_Interval(a,b))
    case (ASome(a), AMaybe(b)) => AMaybe(intervals.union_Interval(a,b))
    case (AMaybe(a), ASome(b)) => AMaybe(intervals.union_Interval(a,b))
    case (AMaybe(a), AMaybe(b)) => AMaybe(intervals.union_Interval(a,b))
  }

  def union_AOption_AList(ao1: AOption[AList], ao2: AOption[AList]) : AOption[AList] = (ao1,ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, AMaybe(e)) => AMaybe(e)
    case (AMaybe(e), ANone) => AMaybe(e)
    case (ANone, ASome(e)) => AMaybe(e)
    case (ASome(e), ANone) => AMaybe(e)
    case (ASome(a), ASome(b)) => ASome(union_AList(a,b))
    case (ASome(a), AMaybe(b)) => AMaybe(union_AList(a,b))
    case (AMaybe(a), ASome(b)) => AMaybe(union_AList(a,b))
    case (AMaybe(a), AMaybe(b)) => AMaybe(union_AList(a,b))
  }

  def union_ABool(ab1: ABool, ab2: ABool) : Set[ABool] = (ab1, ab2) match {
    case (AFalse,AFalse) => Set(AFalse)
    case (ATrue, ATrue)  => Set(ATrue)
    case (ATrue,AFalse) | (AFalse, ATrue) => Set(AFalse,ATrue)
  }


  def intersect_AList(al1: AList, al2: AList) : AList = (al1, al2) match {
    case (ANil, ANil) => ANil
    case (ANil, AMany(_)) | (AMany(_), ANil) => ANil
    case (ANil, ACons(_,_)) | (ACons(_,_), ANil) => ANil
    case (AMany(a), AMany(b)) =>
      if(intervals.intersect_Interval(a,b) != intervals.Interval(IntegerInf, IntegerNegInf)) AMany(intervals.intersect_Interval(a,b)) else ANil
    case (ACons(a,as), AMany(e)) =>
      if(intervals.intersect_Interval(a,e) != intervals.Interval(IntegerInf, IntegerNegInf)) ACons(intervals.intersect_Interval(a,e), intersect_AList(as, al2)) else ANil
    case (AMany(e), ACons(a,as)) =>
      if(intervals.intersect_Interval(e,a) != intervals.Interval(IntegerInf, IntegerNegInf)) ACons(intervals.intersect_Interval(e,a), intersect_AList(al1, as)) else ANil
    case (ACons(a,as), ACons(b, bs)) =>
      if(intervals.intersect_Interval(a,b) != intervals.Interval(IntegerInf, IntegerNegInf)) ACons(intervals.intersect_Interval(a,b),intersect_AList(as, bs)) else ANil
  }

  //TODO intersect_AOption[AInt]
  //TODO intersect_AOption[AList]

  def intersect_ABool(ab1: ABool, ab2: ABool) : Set[ABool] = (ab1, ab2) match {
    case (AFalse,AFalse) => Set(AFalse)
    case (ATrue, ATrue) => Set(ATrue)
    case (ATrue,AFalse) | (AFalse, ATrue) => Set()
  }


  //left AList is subset of right AList
  def subset_AList(al1: AList, al2: AList) : Boolean = (al1, al2) match {
    case (ANil, ANil) => true
    case (ANil, AMany(_)) => true
    case (AMany(_), ANil) => true
    case (ANil, ACons(_,_)) => true
    case (ACons(_,_), ANil) => false
    case (AMany(a), AMany(b)) => intervals.contains_Interval(a,b)
    case (ACons(a,as), AMany(e)) => intervals.contains_Interval(a,e) && subset_AList(as, al2)
    case (AMany(e), ACons(a,as)) => intervals.contains_Interval(e,a) && subset_AList(al1, as)
    case (ACons(a,as), ACons(b, bs)) => intervals.contains_Interval(a,b) && subset_AList(as, bs)
  }

  //TODO subset_Aoption[AInt]
  //TODO subset_Aoption[AList]
  //TODO subset_ABool


  //widen -> not symmetric
  def widen_AList(al1: AList, al2: AList) : AList = (al1, al2) match {
    case (ANil, ANil) => ANil
    case (ANil, AMany(e)) => AMany(e)
    case (AMany(e), ANil) => AMany(e)
    case (ANil, ACons(a, as)) => widen_AList(AMany(a),as)
    case (ACons(a, as), ANil) => widen_AList(AMany(a),as)
    case (AMany(a), AMany(b)) => AMany(intervals.Lattice.widen(a,b))
    case (AMany(e), ACons(b, bs)) => widen_AList(AMany(intervals.Lattice.widen(e,b)), bs)
    case (ACons(a,as), AMany(e)) => widen_AList(AMany(intervals.Lattice.widen(a,e)), as)
    case (ACons(a,as), ACons(b, bs)) => ACons(intervals.Lattice.widen(a,b), widen_AList(as,bs))
  }

  def widen_AOption(ao1: AOption[AInt], ao2: AOption[AInt]) : AOption[AInt] = (ao1, ao2) match {
    case (ANone, ANone) => ANone
    case (ANone, ASome(e)) => AMaybe(e)
    case (ASome(e), ANone) => AMaybe(e)
    case (ANone, AMaybe(e)) => AMaybe(e)
    case (AMaybe(e), ANone) => AMaybe(e)
    case (AMaybe(a), AMaybe(b)) => AMaybe(intervals.Lattice.widen(a,b))
    case (ASome(a), ASome(b)) => ASome(intervals.Lattice.widen(a,b))
    case (AMaybe(a), ASome(b)) => AMaybe(intervals.Lattice.widen(a,b))
    case (ASome(a), AMaybe(b)) => AMaybe(intervals.Lattice.widen(a,b))
  }

  //TODO widen_AOption[AList]
  //TODO widen_ABool

  //TODO recheck all
  def &&(a : ABool, b: ABool) : ABool = (a,b) match {
    case (AFalse, AFalse)| (ATrue, ATrue)  => ATrue
    case (AFalse, ATrue) | (ATrue, AFalse)  => AFalse
    case (AUnknown, AUnknown) => ATrue //TODO recheck AFalse or ATrue
  }

  def ===(a : ABool, b: ABool) : ABool = (a,b) match {
    case (AFalse, AFalse)| (ATrue, ATrue) => ATrue
    case (AFalse, ATrue) | (ATrue, AFalse)  => AFalse
    case (AUnknown, ATrue) | (ATrue, AUnknown) | (AUnknown, AFalse) | (AFalse, AUnknown)=> ATrue
  }

  def ===[AInt](a : AInt, b: AInt) : ABool =  {
    if(a.equals(b)){
      ATrue
    }else{
      AFalse
    }
  }

  //TODO rename to ===
  def equals_AList(l1 : AList, l2: AList) : ABool = (l1,l2) match {
    case (ANil, ANil) => ATrue
    case (ANil, ACons(_,_)) | (ACons(_,_), ANil) => AFalse
    case (ANil, AMany(_)) | (AMany(_), ANil) => ATrue
    case (ACons(a, as), ACons(b,bs)) => &&(===(a,b), equals_AList(as,bs))
    case (AMany(a), AMany(b)) => ===(a,b)
    case (AMany(a), ACons(b,bs)) => &&(===(a,b), equals_AList(l1,bs))
    case (ACons(a,as), AMany(b)) => &&(===(a,b), equals_AList(as,l2))
  }

  //TODO rename to ===
  def equals_AOption_AInt(ao1 : AOption[AInt], ao2: AOption[AInt]) : ABool = (ao1,ao2) match {
    case (ANone, ANone) => ATrue
    case (ANone, ASome(_)) | (ASome(_), ANone) => AFalse
    case (ANone, AMaybe(_))| (AMaybe(_), ANone) => ATrue
    case (ASome(a), ASome(b)) => ===(a,b)
    case (AMaybe(a), AMaybe(b)) =>  ===(a,b)
    case (ASome(a), AMaybe(b))  =>  ===(a,b) //TODO  exclude ANone
    case (AMaybe(a), ASome(b)) => ===(a,b) //TODO  exclude ANone

  }

  //TODO rename to ===
  def equals_AOption_AList(ao1 : AOption[AList], ao2: AOption[AList]) : ABool = (ao1,ao2) match {
    case (ANone, ANone) => ATrue
    case (ANone, ASome(_))| (ASome(_), ANone) => AFalse
    case (ANone, AMaybe(_))| (AMaybe(_), ANone) => ATrue
    case (ASome(a), ASome(b)) => equals_AList(a,b)
    case (AMaybe(a), AMaybe(b)) =>  equals_AList(a,b)
    case (ASome(a), AMaybe(b))   =>  equals_AList(a,b) //TODO  exclude ANone
    case (AMaybe(a), ASome(b)) => equals_AList(a,b) //TODO  exclude ANone
  }


  def !==(ao1: ABool, ao2: ABool) : ABool = (ao1,ao2) match {
    case (AFalse, AFalse) | (ATrue, ATrue) => AFalse
    case (AFalse, ATrue) | (ATrue, AFalse) => ATrue
  }

  //TODO ABool -> Bool Translator


  //TODO Hilfsfunktion für Loops
  //TODO wie mit ANone umgehen? -> Check vor Methoden-Aufruf
  def justAList(ao: AOption[AList]) : AList = ao match {
    case ASome(e) => e
    case AMaybe(e)  => e
  }




  implicit def Lattice: Lattice[AList] = new Lattice[AList] {

    override def bot: AList = ACons(intervals.Lattice.bot, ANil)

    override def top: AList = AMany(intervals.Lattice.top)

    override def lub(a1: AList, a2: AList): AList = intersect_AList(a1: AList, a2: AList)

    override def glb(a1: AList, a2: AList): AList = union_AList(a1: AList, a2: AList)

    override def <=(a1: AList, a2: AList): Boolean = subset_AList(a1: AList, a2: AList)

    override def widen(a1: AList, a2: AList, bound: Int): AList = widen_AList(a1: AList, a2: AList)

  }













  //TODO recheck + Doku + own class

  case class AState(n:AInt, xs:AList)   //e.g. while loop with n = 0, xs = AMany(e)

  //Abstraction: Statement
  trait AStmt{
    def execute(as: AState) : Set[AState] //TODO Parameter: Set[AState] or just AState
  }


  //Object of trait AStmt
  object AssignN0 extends AStmt{  //initial, beginning of loop
    override def execute(as:AState): Set[AState]  = {
        Set(AState(Interval(IntegerVal(0), IntegerVal(0)), as.xs))
    }
  }

  object AssignN1 extends AStmt{
    override def execute(as:AState): Set[AState]  = {
      Set(AState(Interval(IntegerVal(1), IntegerVal(1)), as.xs))
    }
  }

  //
  object AssignN_Test1 extends AStmt{
    override def execute(as:AState): Set[AState]  = {
      Set(AState(intervals.-(as.n, Interval(IntegerVal(1), IntegerVal(1))), justAList(aTail(as.xs))))
    }
  }
  object AssignN_Test2 extends AStmt{
    override def execute(as:AState): Set[AState]  = {
      Set(AState(as.n, as.xs))
    }
  }

  case class Sequence(stmt1: AStmt, stmt2: AStmt){
    def execute(as0: AState): Unit ={
      for(as1 <- stmt1.execute(as0);
          as2 <- stmt2.execute(as1))
          yield as2
    }
  }

  //Method splits a given AList into two sets (Empty AList, Non-Empty AList)
  def ifIsNil(l:AList) : (Set[AList], Set[AList]) = l match {
    case ANil => (Set(ANil), Set())
    case ACons(h,t) => (Set(), Set(ACons(h,t)))
    case AMany(e) => (Set(ANil), Set(ACons(e, AMany(e))))
  }

  //TODO recheck
  //Abstracted if-else Implementation
  case class IfxsIsNil(stmt1: AStmt, stmt2: AStmt){
    def execute(as0: AState) = {
      val (aStatesTrue, aStatesFalse) = ifIsNil(as0.xs)
      val result1 = for(as1 <- aStatesTrue; as2 <- stmt1.execute(as0)) yield as2
      val result2 = for(as1 <- aStatesFalse; as2 <- stmt2.execute(as0)) yield as2
      result1.union(result2)
    }
  }


}










