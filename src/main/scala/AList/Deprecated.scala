package AList

class Deprecated {



  //TODO deprecated
  /*
  case class AAssignFirst(first: Any) extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      for(AState(_,second) <- as) yield AState(first,second)
    }
  }


//TODO deprecated
  //Statements assigns interval [0;0] to a Set of AStates -> initialState
  object AssignN0 extends AStmt { //initial, beginning of loop
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        if(a.first.isInstanceOf[AInt]) {
          result += AState(Interval(IntegerVal(0), IntegerVal(0)), a.second)
        } else{
          ???
        }
      }
      result
    }
  }
//TODO deprecated
  //Statements assigns a interval [1;1] to a Set of AStates -> initialState
  object AssignN1 extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        if(a.first.isInstanceOf[AInt] ) {
          result += AState(Interval(IntegerVal(1), IntegerVal(1)), a.second)
        } else{
          ???
        }
      }
      result
    }
  }

//TODO deprecated
  //Statements assigns AFalse to a Set of AStates -> initialState
  object AssignAFalse extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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

//TODO deprecated
  //Statements assigns ATrue to a Set of AStates -> initialState
  object AssignATrue extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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
*/ //TODO deprecated until here

  /* //TODO deprecatet --> use APlus
  //Statements adds interval [1;1] to a Set of AStates
  object Add1 extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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

//TODO deprecated
  //Statements subtracts a interval [1;1] off a Set of AStates
  object Subtract1 extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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

  //TODO deprecated
  object Subtract1_ATail extends AStmt {
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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
    override def execute(as: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (a <- as) {
        result += a
      }
      result
    }
  }




   */

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


  /*
 //TODO deprecated
 //trait "separates" the input of a unary operations in two sets
 trait AUnOp[A]{
   def positive(a:A): Set[A]
   def negative(a:A): Set[A]
 }
//TODO deprecated -> need new version
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





  */
  /*
   //TODO deprecated
    //trait "separates" inputs of unary operations in two sets
    trait ABinOp[A]{
      def positive(a1:A,a2: A ) :  Set[AInt]
      def negative(a1:A,a2: A) : (Set[(A,A)] , Set[(A,A)])
    }
   //TODO deprecated

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


   */

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






  /*

//TODO: needs improvement
  def AStateEqual(as1: AState, as2: AState): ABool = {
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


   */



/*
  object xsIsNilTest extends ATest {
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {       //1. check if it is an AList
                                    //2. add this state to the result
        //TODO multiple AList?
        if(state.values.exists(_._1 == "AList")){
          val (isNil, _) = ifIsNil(state.lookup("AList").asInstanceOf[AList]) //TODO alternative?
          result ++= isNil.map(state)
        }

       // val (isNil, _) = ifIsNil(???)
        // result ++= isNil.map(state)
      }
     result
    }



    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        val (_, isNotNil) = ifIsNil(state.xs)
        result ++= isNotNil.map(state)
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
/*
  object nIsPositive extends ATest {
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if intervals.isPositive(int) =>
            result += state
          case _ =>
        }
      }
      result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if intervals.isNegative(int) =>
            result += state
          case _ =>
        }
      }
      result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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
    override def positive(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
      for (state <- states) {
        state.first match {
          case int: AInt if intervals.isZero(int) =>
            result += state
          case _ =>
        }
      }
      result
    }

    override def negative(states: Set[AState]): Set[AState] = {
      var result: Set[AState] = Set()
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



 */
}
