package aima.logic.fol

/** Returns given Sentence/Clause/Term.. after making the
 * substitutions given in theta.
 *
 * @author Himanshu Gupta
 */
object Subst {

  def apply[T](theta: Map[Variable,Term], alpha: T): T =
    alpha match {
      case x: Term => substTerm(theta,x).asInstanceOf[T]
      case x: AtomicSentence => substAtomicSentence(theta,x).asInstanceOf[T]
      case x: Sentence => substComplexSentence(theta,x).asInstanceOf[T]
      case x: Clause => substClause(theta,x).asInstanceOf[T]
      case x: Literal => substLiteral(theta,x).asInstanceOf[T]

      //for a Set of something
      case x: Set[_] =>
        x.map(apply(theta,_)).asInstanceOf[T]
    }

  //for Term
  private def substTerm(theta: Map[Variable,Term], alpha: Term): Term =
    alpha match {
      case x: Constant => x
      case x: Variable =>
        if(theta.contains(x))
          theta(x)
        else x
      case x: Function => new Function(x.symbol, x.args.map(substTerm(theta,_)):_*)
    }

  //for Atomic Sentence
  private def substAtomicSentence(theta: Map[Variable,Term], alpha: AtomicSentence): AtomicSentence =
    alpha match {
      case x: Predicate => new Predicate(x.symbol, x.args.map(substTerm(theta,_)):_*)
      case x: Equal => new Equal(substTerm(theta,x.lTerm),substTerm(theta,x.rTerm))
    }

  //for Complex Sentence
  private def substComplexSentence(theta: Map[Variable,Term], alpha: Sentence): Sentence =
    alpha match {
      case x: AtomicSentence => substAtomicSentence(theta,x)
      case x: Negation =>
        new Negation(substComplexSentence(theta,x.sentence))
      case x: Conjunction =>
        new Conjunction(x.conjuncts.map(substComplexSentence(theta,_)).toList:_*)
      case x: Disjunction =>
        new Disjunction(x.disjuncts.map(substComplexSentence(theta,_)).toList:_*)
      case x: Conditional =>
        new Conditional(substComplexSentence(theta,x.premise),substComplexSentence(theta,x.conclusion))
      case x: BiConditional =>
        new BiConditional(substComplexSentence(theta,x.condition),substComplexSentence(theta,x.conclusion))
      case x: Quantifier if theta.contains(x.variable) =>
          throw new IllegalStateException(x.variable + " should not occur in " + theta)
      case x: UniversalQuantifier =>
          new UniversalQuantifier(x.variable,substComplexSentence(theta,x.sentence))
      case x: ExistentialQuantifier =>
          new ExistentialQuantifier(x.variable,substComplexSentence(theta,x.sentence))
    }

  //for Positive/Negative Literal
  private def substLiteral(theta: Map[Variable,Term], alpha: Literal): Literal =
    alpha match {
      case x: PositiveLiteral => PositiveLiteral(substAtomicSentence(theta,x.sentence))
      case x: NegativeLiteral => NegativeLiteral(substAtomicSentence(theta,x.sentence))
    }

  //for Clause
  private def substClause(theta: Map[Variable,Term], alpha: Clause): Clause =
    new Clause(alpha.literals.map(substLiteral(theta,_)).toList:_*)
}

/** UNIFY, described in Fig 9.1
 *
 * @author Himanshu Gupta
 */
object Unify {

  def apply[T](x: T, y: T): Option[Map[Variable,Term]] = apply(x,y,Some(Map[Variable,Term]()))

  def apply[T](x: T, y: T, theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    x match {
      case p: Term => unifyTerm(x.asInstanceOf[Term],y.asInstanceOf[Term],theta)
      case p: AtomicSentence =>
        unifyAtomicSentence(x.asInstanceOf[AtomicSentence],y.asInstanceOf[AtomicSentence],theta)
      case p: List[Term] =>
        unifyTermList(x.asInstanceOf[List[Term]],y.asInstanceOf[List[Term]],theta)
    }

  private def unifyTerm(x: Term, y: Term, theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    (theta,x,y) match {
      case (None,_,_) => None //failure
      case (_,_,_) if x == y => theta
      case (_, a: Variable,_) => unifyVar(a,y,theta)
      case (_,_,a: Variable) => unifyVar(a,x,theta)
      case (_,a:Function,b:Function) =>
          if(a.symbol == b.symbol)
            unifyTermList(a.args,b.args,theta)
          else None
      case _ => None     
  }

  private def unifyTermList(x: List[Term], y: List[Term], theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    theta match {
      case None => None //failure
      case Some(m) =>
        (x,y) match {
          case (aX :: restX, aY :: restY) => unifyTermList(restX,restY,unifyTerm(aX,aY,theta))
          case (Nil, Nil) => theta
          case _ => None
        }
    }

  private def unifyAtomicSentence(x: AtomicSentence, y: AtomicSentence, theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    (x,y) match {
      case (a: Predicate, b: Predicate) =>
        if(a.symbol == b.symbol) unifyTermList(a.args,b.args,theta)
        else None
      case (a: Equal, b: Equal) =>
        val theta1 = unifyTerm(a.lTerm,b.lTerm,theta)
      val theta2 = unifyTerm(a.rTerm,b.rTerm,theta)
      (theta1, theta2) match {
        case (None,_) => None
        case (_,None) => None
        case (Some(m1),Some(m2)) => merge(m1,m2)
      }
      case _ => None
    }

  private def unifyVar(v: Variable, x: Term, theta: Option[Map[Variable,Term]]): Option[Map[Variable,Term]] =
    theta match {
      case None => None //failure
      case Some(m) if m.contains(v) => apply(m(v),x,theta)
      case Some(m) =>
        x match {
          case a: Variable if m.contains(a) => apply(v,m(a),theta)
          //TODO: case occur-check => failure
          case _ => Some(m + (v -> x))
        }
    }

  //Merge(take Union) two unifiers if they don't conflict(that is don't have different
  //values for same variables)
  def merge(m1: Map[Variable,Term], m2: Map[Variable,Term]): Option[Map[Variable,Term]] = {
    //see if m1 and m2 have conflict
    val doConflict = m1.exists(a => {
      val (k,v) = a
      m2.contains(k) && (v != m2(k))})

    if(doConflict) None
    else Some(m1 ++ m2)
  }
}


/** Collects all variables from given sentence/term/clause..
 *
 * @author Himanshu Gupta
 */
object CollectVariables {

  def apply[T](s: T): Set[Variable] =
    s match {
      case x: Term => fromTerm(x)
      case x: AtomicSentence => fromAtomicSentence(x)
      case x: Clause => fromClause(x)
      case x: FOLDefiniteClause => fromDefiniteClause(x)
    }
      
  private def fromAtomicSentence(s: AtomicSentence): Set[Variable] =
    s match {
      case x: Predicate =>
        Set(x.args.flatMap(fromTerm(_)):_*)
      case x: Equal =>
        fromTerm(x.lTerm) ++ fromTerm(x.rTerm)
    }

  private def fromTerm(t: Term): Set[Variable] =
    t match {
      case x: Constant => Set[Variable]()
      case x: Variable => Set(x)
      case x: Function => Set(x.args.flatMap(fromTerm(_)):_*)
    }

  private def fromClause(clause: Clause): Set[Variable] =
    clause.literals.flatMap(l => fromAtomicSentence(l.sentence))

  private def fromDefiniteClause(clause: FOLDefiniteClause): Set[Variable] =
    clause match {
      case x: AtomicSentence =>
        fromAtomicSentence(x)
      case x: ImplicationDefiniteClause =>
        x.premise.flatMap(fromAtomicSentence(_)) ++
        fromAtomicSentence(x.conclusion)
    }
}
