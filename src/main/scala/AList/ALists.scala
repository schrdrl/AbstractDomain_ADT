package AList

import Console.{GREEN, RED, RESET, YELLOW}

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



  /************************************************************
   *                Advanced Functions over ALists            *
   ************************************************************/

   //Method prepends an element on the front a an AList value
   def +:(elem: AInt, al2: AList) : AList = ACons(elem, al2 )

  //Method appends an element the the end a an AList value
   def :+(al1: AList, elem: AInt) : AList = al1 match {
     case ANil => ACons(elem, ANil)
     case ACons(h,t) => ACons(h,:+(t, elem))
     case AMany(e) => AMany(intervals.union_Interval(e, elem))  //TODO recheck

   }


  //Method concatenates two values of type AList
   def ++(al1: AList, al2: AList): AList = (al1, al2) match {
     case (ANil, ANil) =>ANil
     case (ANil, ACons(h,t)) => ACons(h,t)
     case (ACons(h,t), ANil) => ACons(h,t)
     case (ANil, AMany(e)) => AMany(e)
     case (AMany(e), ANil) => AMany(e)
     case (AMany(e1), AMany(e2)) => AMany(intervals.union_Interval(e1,e2))
     case (AMany(e1), ACons(h,t)) => union_AList(al1, flatten_AList(al2))   //TODO recheck
     case (ACons(h,t), AMany(e2)) => ACons(h, ++(t, al2))
     case (ACons(h1, t1), ACons(h2, t2)) => ACons(h1, ++(t1, al2))
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


  //widening of two AStates at the specified keys
  def widen_AState(as1: AState, as2 : AState, keys: Set[String]) : AState = {
    var state : AState = as1
    for (op <- keys){
      op match {
        case "AInt" =>  state = AAssign("AInt", AConst(intervals.Lattice.widen(state.lookup("AInt").asInstanceOf[AInt], as2.lookup("AInt").asInstanceOf[AInt]))).execute(Set(state,as2)).head
        case "AList" => state = AAssign("AList", AConst(widen_AList(state.lookup("AList").asInstanceOf[AList], as2.lookup("AList").asInstanceOf[AList]))).execute(Set(state,as2)).head
       //TODO
      }
    }
    state
  }



  //TODO recheck -> widen or union_AList
  //Method flattens a given AList value (not empty) to a more compact AMany-format
  def flatten_AList(al: AList) : AList = al match {
    case ANil => ANil
    case ACons(h,t) => union_AList(AMany(h), t)
    case AMany(e) => AMany(e)
  }


  //method reverses a given AList
  def reverse(al: AList) : AList = al match {
    case ANil => ANil
    case AMany(e) => AMany(e)
    case ACons(h,t) =>
      var axs : AList = al
      var ays : AList = ANil
      var tailIsAMany = false

      while(ifIsNotNil(axs)._1.nonEmpty && !tailIsAMany){

        val head = justValue(aHead(axs)).head
        val tail = justValue(aTail(axs))
        if(head.values.exists(_._1 == "ASome") && tail.head.values.exists(_._1 == "ASome") && tail.tail.isEmpty){
          ays = ACons(head.lookup("ASome").asInstanceOf[AInt], ays)
          axs = tail.head.lookup("ASome").asInstanceOf[AList]
        }else if(tail.head.values.exists(_._1 == "ASome") && tail.tail.head.values.exists(_._1 == "ANone")){
            ays = flatten_AList(al)
            tailIsAMany = true
        }else{
          throw new Exception("Exception thrown from reverse. Reason: aHead was ANone")
        }
      }
      ays
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


  def ===(as1: AState, as2: AState, keys: Set[String]) : ABool = {
    var isEqual : ABool= ATrue
    for(key <- keys){
      key match {
        case "AInt" => isEqual = ===(as1.lookup("AInt").asInstanceOf[AInt], as2.lookup("AInt").asInstanceOf[AInt])
        case "AList" => isEqual = ===(as1.lookup("AList").asInstanceOf[AList], as2.lookup("AList").asInstanceOf[AList])
        case "ABool" => isEqual = ===(as1.lookup("ABool").asInstanceOf[ABool], as2.lookup("ABool").asInstanceOf[ABool])
        case "AOption[AInt]" => isEqual = ===(as1.lookup("AOption[AInt]").asInstanceOf[AOption[AInt]], as2.lookup("AOption[AInt]").asInstanceOf[AOption[AInt]])
        case "AOption[AList]" => isEqual = ===(as1.lookup("AOption[AList]").asInstanceOf[AOption[AList]], as2.lookup("AOption[AList]").asInstanceOf[AOption[AList]])
      }
      if(isConcreteElementOf_ABool(b = false, isEqual)){
        return AFalse
      }
    }
    isEqual
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
    case ASome(e) =>  Set(AState(Map(("ASome", ASome(e).get))))
    case ANone => Set(AState(Map(("ANone", ANone))))   // throw new Exception("Exception thrown from justAList. Reason: Input was ANone")
    case AMaybe(e) => Set(AState(Map(("ASome", ASome(e).get))), AState(Map(("ANone", ANone))))  // throw new Exception("Exception thrown from justAList. Reason: Input was AMaybe")
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
   *                          AExpr                           *
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
        case(l: AList, "intersect", r: AList) => intersect_AList(l,r)
        case(l: AInt, "intersect", r: AInt) => intervals.intersect_Interval(l,r)
        case(l: AOption[AInt], "intersect", r: AOption[AInt]) => intersect_AOption_AInt(l,r)
        case(l: AOption[AList], "intersect", r: AOption[AList]) => intersect_AOption_AList(l,r)
        case(l: AList, "subset", r: AList) => subset_AList(l,r)
        case(l: AList, "widen", r: AList) => widen_AList(l,r)
        //TODO
        /*
        Ideas: AAssume
        case ("<", ae: AInt) => ???
        case (">", ae: AInt) => ???
        case ("<=", ae: AInt) => ???
        case (">=", ae: AInt) => ???
        case ("==", ae: AInt) => ???
        case ("!=", ae: AInt) => ???
        case ("==", ae: AList) => ???
        case ("!=", ae: AList) => ???
        case ("==", ae: ABool) => ???
        case ("!=", ae: ABool) => ???
         */
      }

    }
  }

  //Unary operators
  case class AUnOp(op: String, aexpr: AExpr) extends AExpr {
    override def evaluate(as: AState): Any = {
      val ae = aexpr.evaluate(as)
      (op,ae) match {
        case ("aHead" ,ae: AList) => justValue(aHead(ae))
        case ("aTail", ae: AList) => justValue(aTail(ae))
        case ("aLength", ae: AList) => justValue(aLength(ae))
        //TODO
      }

    }
  }

  //test operators
  case class ATestOp(op: String, aexpr: AExpr) extends AExpr {
    override def evaluate(as: AState): Any = {
      val ae = aexpr.evaluate(as)
      (op,ae) match {
        case("ifIsNil", ae: AList) => ifIsNil(ae)
        case("ifIsNotNil", ae: AList) => ifIsNotNil(ae)
        case("ifIsPositive", ae:AInt) => ifIsPositive(ae)
        case("ifIsNegative", ae:AInt) => ifIsNegative(ae)
        case("ifIsZero", ae:AInt) => ifIsZero(ae)
        case("ifIsATrue", ae:ABool) => ifIsATrue(ae)
        case("ifIsAFalse", ae:ABool) => ifIsAFalse(ae)
        //ifIsEqual_AInt
        //ifIsEqual_AList
      }
    }
  }



  /************************************************************
   *                          AStmt                           *
   ************************************************************/

  /**
   * Abstracted statements.
   * expression of type Set[AState] with e.g
   *    AUnOp: AState ("AUnOp", "aTail"), ("operand", "AList")
   *    ABinOp: AState("ABinOp", "-"), ("operator", [1;1]), ("operand", "AInt")
   *
   * will return a Set[AState] which contains the updated input AStates
   */
   case class AStmt(expression : Set[AState]){
    def execute(as: Set[AState]) : Set[AState] = {
      var result : Set[AState] = Set()  //return
      for(a <- as) {
        var result_state : AState= a
        for(expr <- expression){
          if(expr.values.exists(_._1 == "AUnOp")){
            //evaluate expression
            val value = AUnOp( expr.lookup("AUnOp").asInstanceOf[String], AConst(result_state.lookup(expr.lookup("operand").asInstanceOf[String]))).evaluate(result_state)
            for (v <- value.asInstanceOf[Set[AState]]){
              if(v.values.exists(_._1 == "ASome")){
                //update AState
                result_state = AAssign(expr.lookup("operand").asInstanceOf[String], AConst(v.lookup("ASome"))).execute(Set(result_state)).head
              }
            }
          }else if(expr.values.exists(_._1 == "ABinOp")){
            val value = ABinOp(AConst(result_state.lookup(expr.lookup("operand").asInstanceOf[String])), expr.lookup("ABinOp").asInstanceOf[String], AConst(expr.lookup("operator")) ).evaluate(result_state)
            result_state = AAssign(expr.lookup("operand").asInstanceOf[String], AConst(value)).execute(Set(result_state)).head
          }
        } //end of expression
        result += result_state  //assign results of current state to overall result
      } //end of as
      result
    }
  }


  //Method collects operands used in a Set of AStates of AStmt
  def getOperands(expressions:Set[AState]) : Set[String] = {
    var result : Set[String] = Set()
    for(expr <- expressions){
      if(expr.values.exists(_._1 == "operand")){
        result += expr.lookup("operand").asInstanceOf[String]
      }
    }
    result
  }

  /************************************************************
   *                          Tests                           *
   ************************************************************/


  // checks whether a given AList is Nil
  def isNil(l: AList): (Set[ABool],Set[ABool]) = l match {
    case ANil => (Set(ATrue), Set())
    case ACons(_, _) =>  ( Set(), Set(AFalse))
    case AMany(_) => (Set(ATrue),Set(AFalse))
  }

  //Method splits a given AList into two sets (Empty AList, Non-Empty AList)
  def ifIsNil(l: AList): (Set[AList], Set[AList]) = l match {
    case ANil => (Set(ANil), Set())
    case ACons(h, t) => (Set(), Set(ACons(h, t)))
    case AMany(e) => (Set(ANil), Set(ACons(e, AMany(e))))
  }

  //Method splits a given AList into two sets (Non-Empty AList, Empty AList)
  def ifIsNotNil(l: AList): (Set[AList], Set[AList]) = l match {
    case ANil => (Set(), Set(ANil))
    case ACons(h, t) => (Set(ACons(h, t)), Set())
    case AMany(e) => (Set(ACons(e, AMany(e))), Set(ANil))
  }

  def ifIsPositive(ai :AInt): (Set[AInt], Set[AInt]) ={
    if(intervals.<=(Interval(IntegerVal(0), IntegerVal(0)), ai)){  //ai >= 0
      (Set(ai), Set())
    }else if(IntegerW.<=(ai.lb, IntegerVal(0))&& IntegerW.<(IntegerVal(0), ai.ub)){  //ub > 0 && lb < 0
      (Set(Interval(IntegerVal(0), ai.ub)), Set(Interval(ai.lb, IntegerVal(-1))))
    }else  {//lb < 0 && ub <0 //if(intervals.<(ai, Interval(IntegerVal(0), IntegerVal(0))))
     (Set(), Set(ai))
    }
  }


  def ifIsNegative(ai :AInt): (Set[AInt], Set[AInt]) = {
    if(intervals.<=(ai,  Interval(IntegerVal(0), IntegerVal(0)))){  //ai <= 0
      (Set(ai), Set())
    }else if(IntegerW.<=(ai.lb, IntegerVal(0))&& IntegerW.<(IntegerVal(0), ai.ub)){  //ub > 0 && lb < 0
      (Set(Interval(ai.lb, IntegerVal(0))), Set(Interval(IntegerVal(1), ai.ub)))
    }else  {//lb > 0 && ub >0 //if(intervals.<(Interval(IntegerVal(0), IntegerVal(0)), ai))
      (Set(), Set(ai))
    }
  }

  def ifIsZero(ai :AInt): (Set[AInt], Set[AInt]) = {
    if(intervals.===(ai, Interval(IntegerVal(0), IntegerVal(0)))){  //ai == 0
      (Set(ai), Set())
    }else if (IntegerW.<(ai.lb, IntegerVal(0))&& IntegerW.<(IntegerVal(0), ai.ub)) { //lb <0 && ub > 0
      (Set(Interval(IntegerVal(0), IntegerVal(0))), Set(Interval(ai.lb, IntegerVal(-1)), Interval(IntegerVal(1), ai.ub)))
    }else{ //ai < 0 || ai > 0
      (Set(), Set(ai))
    }
  }

  def ifIsATrue(ab: ABool): (Set[ABool], Set[ABool]) = ab match{
    case ATrue => (Set(ATrue), Set())
    case AFalse => (Set(), Set(AFalse))
  }

  def ifIsAFalse(ab: ABool): (Set[ABool], Set[ABool]) = ab match{
    case ATrue => (Set(), Set(ATrue))
    case AFalse => (Set(AFalse), Set())
  }

  //TODO ifIsEqual_AInt
 // def ifIsEqual_AInt(ai1: AInt, ai2: AInt) : (Set[AInt], Set[AInt]) = {}
  //TODO ifIsEqual_AList

  /************************************************************
   *                          ATest                           *
   ************************************************************/

  /**
   * Abstracted tests.
   * expression of type Set[AState] with e.g
   *    ATestOp: AState ("test", "ifIsNil"), ("testedValue", "AList")
   *
   *    IMPORTANT: tests included in ATestOp must have a return statement of type (Set[], Set[])
   *    (Set[], _) -> success
   *    (_, Set[]) -> fail
   *
   * will return a Set[AState] which contains the updated input AStates
   */
  case class ATest(tests: Set[AState]){
    def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set() //return

      for (state <- states){
        //println("State: "+state)
        var result_state : AState= AState(Map())
        for (test <- tests) {
          //println("Test: "+test)
          val (value,_) = ATestOp(test.lookup("test").asInstanceOf[String],AConst(state.lookup(test.lookup("testedValue").asInstanceOf[String])) ).evaluate(state)
          if(value != Set()) {
            //println("Value: "+value)
            for(v <- value.asInstanceOf[Set[Any]]){
              result_state = AAssign(test.lookup("testedValue").asInstanceOf[String], AConst(v)).execute(Set(state)).head
              result += result_state
            }
          }
        }
      }
      result
    }

    def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set() //return
      for (state <- states){
       // println("State: "+state)
        var result_state : AState= AState(Map())
        for (test <- tests) {
         // println("Test: "+test)
          val (_,value) = ATestOp(test.lookup("test").asInstanceOf[String],AConst(state.lookup(test.lookup("testedValue").asInstanceOf[String])) ).evaluate(state)
          for(v <- value.asInstanceOf[Set[Any]]) {
            if (value != Set()) {
              //println("Value: "+value)
              result_state = AAssign(test.lookup("testedValue").asInstanceOf[String], AConst(v)).execute(Set(state)).head
              result += result_state
            }
          }
        }
      }
      result
    }
  }


  /************************************************************
   *                 Abstract Transformers                    *
   ************************************************************/

  //Abstract transformer: AIf is an abstract representation of an If and If-Else construct
  case class AIf(test: ATest, left: AStmt, right:AStmt){
      def execute(as: Set[AState]): Set[AState] = {
        if(right != AStmt(Set())){ //if else
          left.execute(test.positive(as)) ++ right.execute(test.negative(as))
        }else{  //if
          left.execute(test.positive(as))
        }
    }
  }


  //Abstract transformer: AWhile is an abstract representation of a while loop
  case class AWhile(test: ATest, body: AStmt){
    def execute(as: Set[AState]) : Set[AState] = {
      var states = as
      var result: Set[AState] = Set()
      var afterWiden = AState(Map())
      var stopLoop = false

      while(test.positive(states).nonEmpty && !stopLoop){  //1. test condition
        result = Set()
        val positiveStates = test.positive(states)
         // println("Test outcome (positive): " +positiveStates)

        for(pos <- positiveStates){
         // println("Current AState: " +pos)
          val afterExecution = body.execute(Set(pos))//2. execute body for each state in positive state
          //println("After execution: " +afterExecution)
          afterWiden = widen_AState(pos, afterExecution.head, getOperands(body.expression))//3. widen state and after execution output
         // println("After widening: " +afterWiden)
          result += afterWiden//4. add to result
          //println("Result after widening: " +result)
        }

        if(states == result){ //5. compare whether the result still changes from the states at the loop head
          stopLoop = true
         // println("Loop stops here" +"\n")
        }else{
          states = result
          //println("New state: " +states +"\n")
        }
      }
      result
    }
  }


  //Abstract Transformer: AAssert is an abstract representation of an assertion
  case class AAssert(test: ATest){
    def execute(as: Set[AState]) : (Set[AState], Set[AState]) = {
      (test.positive(as), test.negative(as))
    }
  }

  //Abstract Transformer: AVerify
  case class AVerify(test: ATest){
    def execute(as: Set[AState]) : Unit = {
      for(a <- as){
        if(test.positive(Set(a)).nonEmpty && test.negative(Set(a)).isEmpty){
          Console.println(s"$RESET${GREEN}Assertion fulfills for output-state: $RESET" +a)  //success
          test.positive(Set(a)).foreach(posElement => Console.println(s"$RESET$GREEN $posElement $RESET" +"\n"))
        }else if(test.positive(Set(a)).isEmpty && test.negative(Set(a)).nonEmpty){
          Console.println(s"$RESET${RED}Assertion fails for output-state:$RESET" +a) //fails
          test.negative(Set(a)).foreach(negElement => Console.println(s"$RESET$RED $negElement $RESET"+"\n"))
        }else{ //test.positive(Set(a)).nonEmpty && test.negative(Set(a)).nonEmpty
          Console.println(s"$RESET${YELLOW}Assertion might fulfill for output-state: $RESET" +a)  //success
          test.positive(Set(a)).foreach(posElement => Console.println(s"$RESET$YELLOW $posElement $RESET"))
          Console.println(s"$RESET${YELLOW}Assertion might fail for output-state: $RESET" +a) //fails
          test.negative(Set(a)).foreach(negElement => Console.println(s"$RESET$YELLOW $negElement $RESET"+"\n"))
        }
      }
    }
  }





  //Abstract Transformer: AAssume is an abstract representation of an assumption
  //TODO Testcases + scenarios -> functionality
  //e.g. Boundaries
  case class AAssume(assumption: AStmt){
      def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = assumption.execute(as)
      result
    }
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
   *                     Concretization                       *
   ************************************************************/

  //Method concretizes the abstract type ABool into concrete values of Boolean
  def concretization_ABool(ab: Set[ABool]): Set[Boolean] = ab.map {
    case ATrue => true
    case AFalse => false
  }



}










