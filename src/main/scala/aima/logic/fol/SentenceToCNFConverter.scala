package aima.logic.fol

/** Converts a sentence to CNF Clause Set, it is based on
 * the description given in
 * http://logic.stanford.edu/classes/cs157/2008/lectures/lecture09.pdf
 *
 * @author Himanshu Gupta
 */
object SentenceToCNF {
  def apply(s: Sentence, KB: FOLKnowledgeBase) : Set[Clause] = {

    //Implications out, eliminate all the occurences of
    //=> and <=> ops
    var result = removeImplications(s)

    //Negations In, negations are distributed over other
    //logical operators untill each negation applies only to
    //single Atomic Sentence
    result = negationsIn(result)

    //Standardize-Variables, rename variables so that each
    //quantifier has a unique variable
    result = standardizeQuantifierVariables(result,KB)

    //Existentials out,
    //case-1, if there are no free variables then replace quantifier var with
    //skolem constant, a brand new constant symbol
    //case-2, if there are free variables then replace quantifier var with
    //skolem function, with a band new function symbol, that has free variables 
    //as its arguments
    result = removeExistentialQuantifiers(result,KB)

    //Drop Universal Quatifiers
    result = removeUniversalQuantifiers(result)

    //Operators Out,distribute \/ and /\ so that sentence becomes
    //conjunction of disjunctions and then return Set[Clause]
    var clauses = removeOperators(result)

    //Rename Variables so that no variable appears in more than one clause
    clauses = renameClauseVariables(clauses,KB)

    //return the Clauses
    clauses
  }

  def removeImplications(s: Sentence): Sentence =
    s match {
      case x: AtomicSentence => x
      case x: Negation =>
        new Negation(removeImplications(x.sentence))
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(removeImplications(_)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(removeImplications(_)).toList:_*)
      case x: Conditional =>
        new Disjunction(new Negation(x.premise),x.conclusion)
      case x: BiConditional =>
        new Conjunction(
          new Disjunction(new Negation(x.condition),x.conclusion),
          new Disjunction(x.condition,new Negation(x.conclusion)))
      case x: UniversalQuantifier =>
        new UniversalQuantifier(x.variable,removeImplications(x.sentence))
      case x: ExistentialQuantifier =>
        new ExistentialQuantifier(x.variable,removeImplications(x.sentence))
    }

  def negationsIn(s: Sentence) = {

    def negationsIn(s:Sentence, shouldNegate: Boolean): Sentence =
      if(shouldNegate) negationsIn(negate(s),false)
      else {
        s match {
          case x: AtomicSentence => x
          case x: Negation =>
            x.sentence match {
              case _: AtomicSentence => x
              case _ => negationsIn(x.sentence,true)
            }
          case x: Conjunction =>
            new Conjunction(x.conjuncts.map(negationsIn(_,false)).toList:_*)
          case x: Disjunction =>
            new Disjunction(x.disjuncts.map(negationsIn(_,false)).toList:_*)
          case x: UniversalQuantifier =>
            new UniversalQuantifier(x.variable,negationsIn(x.sentence,false))
          case x: ExistentialQuantifier =>
            new ExistentialQuantifier(x.variable,negationsIn(x.sentence,false))
        }
      }
    negationsIn(s,false)
  }

  //Standardize Quantifier Variables
  def standardizeQuantifierVariables(s: Sentence, KB: FOLKnowledgeBase): Sentence = {

    //standardize the variables for all the quantifiers(if two or more of
    //them share the same variable name) from the Set of Sentences, which
    //are conjuncts/disjuncts of a Conjunction/Disjunction
    def standardizeVariables(ss: Set[Sentence]) = {
      //Separate Quantifiers and Non Quantifiers
      val qs = ss.filter(_ match {
        case _: Quantifier => true
        case _ => false
      }) //Quantifiers

      val nqs = ss.filter(_ match {
        case _: Quantifier => false
        case _ => true
      }) //Non Quantifiers

      //collect groups of Quantifiers with same variable names
      var m = Map[Variable,List[Quantifier]]()

      qs.foreach( c =>
        c match {
          case y: Quantifier if m.contains(y.variable) =>
            m = m + (y.variable -> (y :: m(y.variable)))
          case y: Quantifier =>
            m = m + (y.variable -> List(y))
          case _ => ; //do nothing
        })

      //For a variable, if there are more than one Quantifiers in
      //the group, rename variables in all but one
      m = Map(m.map(a =>  (a: @unchecked) match {
        case (v,q :: Nil) => a
        case (v,q :: rest) =>
          (v,q ::
           rest.map(x => {
             val newVar = KB.generateVariable(x.variable.symbol)
             x match {
               case _: UniversalQuantifier =>
                 new UniversalQuantifier(newVar, Subst(Map(x.variable -> newVar),
                                                       x.sentence))
               case _: ExistentialQuantifier =>
                 new ExistentialQuantifier(newVar, Subst(Map(x.variable -> newVar),
                                                         x.sentence))
             }})
         )
            
      }).toList:_*)

      val renamedQuantifiers: List[Quantifier] = m.values.toList.flatten

      //Return the non quantifiers ++ renamed quantifiers
      nqs ++ Set(renamedQuantifiers:_*)
    }

    s match {
      case x: AtomicSentence => x
      case x: Negation if x.sentence.isInstanceOf[AtomicSentence] => x
      case x: Conjunction =>
        new Conjunction(standardizeVariables(x.conjuncts).map(standardizeQuantifierVariables(_,KB)).toList:_*)
      case x: Disjunction =>
        new Disjunction(standardizeVariables(x.disjuncts).map(standardizeQuantifierVariables(_,KB)).toList:_*)
      case x: UniversalQuantifier =>
        new UniversalQuantifier(x.variable,standardizeQuantifierVariables(x.sentence,KB))
      case x: ExistentialQuantifier =>
        new ExistentialQuantifier(x.variable,standardizeQuantifierVariables(x.sentence,KB))
    }
  }

  //remove Existentials
  def removeExistentialQuantifiers(s: Sentence, KB: FOLKnowledgeBase): Sentence = {

    //collect Free variables from a Sentence
    def collectSentenceFreeVariables(vs: Set[Variable], s: Sentence): Set[Variable] =
      s match {
        case x: Predicate =>
          Set(x.args.flatMap(collectTermFreeVariables(vs,_)):_*)
        case x: Equal => collectTermFreeVariables(vs,x.lTerm) ++ collectTermFreeVariables(vs,x.rTerm)
        case x: Negation =>
          collectSentenceFreeVariables(vs,x.sentence)
        case x: Conjunction =>
          x.conjuncts.flatMap(collectSentenceFreeVariables(vs,_))
        case x: Disjunction =>
          x.disjuncts.flatMap(collectSentenceFreeVariables(vs,_))
        case x: Quantifier =>
          collectSentenceFreeVariables(vs + x.variable, x.sentence)
      }

    //collect Free variables from a Term
    def collectTermFreeVariables(vs: Set[Variable], t: Term): Set[Variable] =
      t match {
        case x: Constant => Set[Variable]()
        case x: Variable =>
          if(vs.exists(x == _)) Set[Variable]() else Set(x)
        case x: Function =>
          Set(x.args.flatMap(collectTermFreeVariables(vs,_)):_*)
      }

    s match {
      case x: AtomicSentence => x
      case x: Negation if x.sentence.isInstanceOf[AtomicSentence] => x
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(removeExistentialQuantifiers(_,KB)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(removeExistentialQuantifiers(_,KB)).toList:_*)
      case x: UniversalQuantifier =>
        new UniversalQuantifier(x.variable,removeExistentialQuantifiers(x.sentence,KB))
      case x: ExistentialQuantifier =>
        val freeVars = collectSentenceFreeVariables(Set[Variable](),x)
        if(freeVars.isEmpty) //case1 -> free variables don't exist
          Subst(Map(x.variable -> KB.generateConstant),
                x.sentence)
        else //case2 -> free variables do exist
          Subst(Map(x.variable -> KB.generateFunction(freeVars.toList)),
                x.sentence)
    }
  }

  def removeUniversalQuantifiers(s: Sentence): Sentence =
    s match {
      case x: AtomicSentence => x
      case x: Negation if x.sentence.isInstanceOf[AtomicSentence] => x
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(removeUniversalQuantifiers(_)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(removeUniversalQuantifiers(_)).toList:_*)
      case x: UniversalQuantifier => removeUniversalQuantifiers(x.sentence) //drop it
    }

  //Remove Operators
  def removeOperators(s: Sentence): Set[Clause] = {
    
    def unionOfTwoClauseSets(cs1: Set[Clause], cs2: Set[Clause]): Set[Clause] =
      for(ci <- cs1; cj <- cs2) yield new Clause(List((ci.literals ++ cj.literals).toList:_*).toList:_*)
    
    s match {
      case x: AtomicSentence => Set(new Clause(PositiveLiteral(x)))
      case x: Negation if x.sentence.isInstanceOf[AtomicSentence] =>
        Set(new Clause(NegativeLiteral(x.sentence.asInstanceOf[AtomicSentence])))
      case x: Conjunction =>
        x.conjuncts.flatMap(removeOperators(_))
      case x: Disjunction =>
        x.disjuncts.map(removeOperators(_)).reduceLeft(unionOfTwoClauseSets(_,_))
    }
  }


  //Rename Variables, so that no two clauses have same variables
  def renameClauseVariables(clauses: Set[Clause], KB: FOLKnowledgeBase): Set[Clause] = {

    clauses.map(c =>
      Subst(Map(CollectVariables(c).map(v => v -> KB.generateVariable(v.symbol)).toList:_*),
            c))
  }
  


  //Returns negation of a sentence
  def negate(s: Sentence): Sentence =
    s match {
      case x: AtomicSentence => new Negation(x)
      case x: Negation => x.sentence
      case x: Conjunction =>
        new Disjunction(x.conjuncts.map(new Negation(_)).toList:_*)
      case x: Disjunction =>
        new Conjunction(x.disjuncts.map(new Negation(_)).toList:_*)
      case x: Conditional =>
        new Conjunction(x.premise,new Negation(x.conclusion))
      case x: BiConditional =>
        new Disjunction(
          new Conjunction(x.condition, new Negation(x.conclusion)),
          new Conjunction(new Negation(x.condition),x.conclusion))
      case x: UniversalQuantifier =>
        new ExistentialQuantifier(x.variable,new Negation(x.sentence))
      case x: ExistentialQuantifier =>
        new UniversalQuantifier(x.variable,new Negation(x.sentence))
    }
}
