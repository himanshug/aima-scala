package aima.logic.propositional

import aima.commons.Utils
import scala.collection.immutable.{Set,Map}

/** TT-ENTAILS, described in Fig 7.10
 *
 * @author Himanshu Gupta
 */
object TTEntails {
  def apply(KB: Conjunction, alpha: Sentence): Boolean = {
    val symbols = KB.symbols ++ alpha.symbols
    ttCheckAll(KB,alpha,symbols.toList,Map[PropositionSymbol,Boolean]())
  }

  private def ttCheckAll(KB: Conjunction, alpha: Sentence,
                         symbols: List[PropositionSymbol],model: Map[PropositionSymbol,Boolean]): Boolean = {
    symbols match {
      case Nil =>
        KB.isTrue(model) match {
          case Some(true) =>
            alpha.isTrue(model) match {
              case Some(x) => x
              case None => throw new IllegalStateException("Model " + model + " does not contain all symbols.")
            }
          case Some(false) => true //when KB is false, always return true
          case None => 
            throw new IllegalStateException("Model " + model + " does not contain all symbols.")
        }
      case first :: rest =>
        (ttCheckAll(KB,alpha,rest,model + (first -> true)) 
         && 
         ttCheckAll(KB,alpha,rest,model + (first -> false)))
    }
  }
}

/** PL-RESOLUTION, described in Fig 7.12
 *
 * @author Himanshu Gupta
 */
object PLResolution {
  def apply(KB: Conjunction, alpha: Sentence): Boolean = {
    val clauses = SentenceToCNF(new Conjunction((KB.conjuncts + new Negation(alpha)).toList:_*)).clauses

    def loop1(clauses: Set[Clause]):Boolean = {
      //sort clauses according to # of literals it contains, basically
      //we give preference to clauses containing smaller # of literals
      //that automatically brings in unit clause preference also
      val list = clauses.toList.sort(_.literals.size < _.literals.size)

      //if loop2 returns None, that means an empty clause is found in the
      //resolvents
      def loop2(list: List[Clause], newSet: Set[Clause]): Option[Set[Clause]] =
        list match {
          case x :: rest =>
            var tmp = Set[Clause]()
            for(y <- rest) {
              val resolvents = plResolve(x,y)
              if(resolvents.exists(_.isEmpty)) return None
              else tmp = tmp ++ resolvents
            }
            loop2(rest, newSet ++ tmp)
          case Nil => Some(newSet)
        }

      loop2(list,Set[Clause]()) match {
        case None => true
        case Some(s) if s.subsetOf(clauses) => false
        case Some(s) => loop1(clauses ++ s)
      }
    }

    loop1(clauses)
  }

  def plResolve(c1: Clause, c2: Clause): Set[Clause] = {
    
    def loop(ls: List[Literal], result: Set[Clause]): Set[Clause] =
      ls match {
        case (l:PositiveLiteral) :: rest =>
          if(c2.literals.exists(_ == NegativeLiteral(l.symbol)))
            loop(rest,result + new Clause(((c1.literals - l) ++ (c2.literals - NegativeLiteral(l.symbol))).toList:_*))
          else
            loop(rest,result)
        case (l:NegativeLiteral) :: rest =>
          if(c2.literals.exists(_ == PositiveLiteral(l.symbol)))
            loop(rest,result + new Clause(((c1.literals - l) ++ (c2.literals - PositiveLiteral(l.symbol))).toList:_*))
          else
            loop(rest,result)
        case Nil => result
      }

    val resolvents = loop(c1.literals.toList,Set.empty)

    //check if a list of literals contain Positive as well as Negative literal
    //for the same symbol
    def isDiscardable(ls: List[Literal]) =
      Utils.pairs(ls).exists( pair =>
                            pair match {
                              case (PositiveLiteral(x),NegativeLiteral(y)) if x == y  => true
                              case (NegativeLiteral(x),PositiveLiteral(y)) if x == y => true
                              case _ => false })
    //discard all such resolvents and return the rest
    resolvents.filter((c:Clause) => !isDiscardable(c.literals.toList))
  }
}

/** PL-FC-ENTAILS?, described in Fig 7.15
 *
 * @author Himanshu Gupta
 */
object PLFCEntails {

  def apply(KB: Set[DefiniteClause],q: PropositionSymbol, knownTrueSymbols: List[PropositionSymbol]): Boolean = {
    
    val count = scala.collection.mutable.Map((KB.map(c => c -> c.premise.size)).toList:_*)
    val inferred = scala.collection.mutable.Map((KB.flatMap(c =>c.premise + c.conclusion).map((_ -> false))).toList:_*)
    val agenda = new scala.collection.mutable.Queue[PropositionSymbol]()
    agenda ++= knownTrueSymbols

    def loop: Boolean = {
      if(agenda.isEmpty) false
      else {
        val p = agenda.dequeue
        if(p == q) true
        else {
          if(!inferred(p)) {
            inferred += (p -> true)
            KB.foreach(c => {
              if(c.premise.contains(p))
                count += (c -> (count(c)-1))
              if(count(c) == 0)
                agenda.enqueue(c.conclusion)
            })
          }
          loop
        }
      }
    }

    loop
  }
}

/** DPLL-SATISFIABLE?, described in Fig 7.17
 *
 * @author Himanshu Gupta
 */
//TODO: not working after upgrading from scala 2.8.1 to 2.9.1
//needs to be fixed
object DPLLSatisfiable {
  def apply(s: Sentence): Boolean =
    DPLL(SentenceToCNF(s).clauses,s.symbols,Map[PropositionSymbol,Boolean]())

  def DPLL(clauses: Set[Clause],symbols: Set[PropositionSymbol],
                   model: Map[PropositionSymbol,Boolean]): Boolean = {
    if (clauses.forall(_.isTrue(model) == Some(true))) return true
    if(clauses.exists(_.isTrue(model) == Some(false))) return false

    FindPureSymbol(symbols,clauses,model) match {
      case Some((p,value)) => DPLL(clauses, symbols - p, model + (p -> value))
      case None =>
        FindUnitClause(clauses,model) match {
          case Some((p,value)) => DPLL(clauses,symbols - p, model + (p -> value))
          case None =>
            val p = symbols.toList(0)
            val rest = symbols - p
            DPLL(clauses,rest,model + (p -> true)) || DPLL(clauses,rest,model + (p -> false))
        }
    }
  }

  def FindPureSymbol(symbols: Set[PropositionSymbol], clauses: Set[Clause], model: Map[PropositionSymbol,Boolean]): Option[(PropositionSymbol,Boolean)] = {

    //returns true, if given symbol appears as a Pure PositiveLiteral in given set of clauses
    def isPurePositiveLiteral(p: PropositionSymbol, clauses: Set[Clause], model: Map[PropositionSymbol,Boolean]) =
      clauses.forall(c =>
        (c.literals.contains(PositiveLiteral(p)),c.literals.contains(NegativeLiteral(p))) match {
          case (_,false) => true
          case (_,true) => c.isTrue(model) == Some(true)
        })

    //Returns true, if given symbol appears as a Pure NegativeLiteral in given set of clauses
    def isPureNegativeLiteral(p: PropositionSymbol, clauses: Set[Clause], model: Map[PropositionSymbol,Boolean]) =
      clauses.forall(c =>
        (c.literals.contains(PositiveLiteral(p)),c.literals.contains(NegativeLiteral(p))) match {
          case (false,_) => true
          case (true,_) => c.isTrue(model) == Some(true)
        })

    symbols.find(isPurePositiveLiteral(_, clauses, model)) match {
      case Some(p) => Some((p,true))
      case None =>
        symbols.find(isPureNegativeLiteral(_, clauses, model)) match {
          case Some(q) => Some((q,false))
          case None => None
        }
    }
  }

  def FindUnitClause(clauses: Set[Clause], model: Map[PropositionSymbol,Boolean]): Option[(PropositionSymbol,Boolean)] = {
    clauses.find( _.literals.filter(_.isTrue(model) == None).size == 1 ) match {
      case None => None
      case Some(c) => {
        val Some(l) = c.literals.find(_.isTrue(model) == None)
        l match {
          case _:PositiveLiteral => Some((l.symbol,true))
          case _:NegativeLiteral => Some((l.symbol,false))
        }}
    }
  }
}

/** WALKSAT, described in Fig 7.18
 *
 * 0.0 <= probability <= 1.0
 * 
 * @author Himanshu Gupta
 */
object WalkSat {

  def apply(clauses: Set[Clause], probability: Double, maxFlips: Int): Option[Map[PropositionSymbol,Boolean]] = {

    val random = new scala.util.Random
    val randomModel = Map(clauses.flatMap(_.symbols).map((_,random.nextBoolean)).toList:_*)
    
    def loop(counter: Int, model: Map[PropositionSymbol,Boolean]): Option[Map[PropositionSymbol,Boolean]] = {
      if (counter < maxFlips) {
        //find clauses that fail in the model
        val failedClauses = clauses.filter(_.isTrue(model) match {
                                            case Some(true) => false
                                            case Some(false) => true
                                            case None =>
                                              throw new IllegalStateException("Model should have all symbols defined.")}).toList
        //if length of failedClauses if 0, that means model satisfies all the
        //clauses
        val len = failedClauses.length
        if(len == 0) Some(model)
        else {
          //randomly select a failed clause
          val clause = failedClauses(random.nextInt(len))
          val symbols = clause.symbols.toList

          if(random.nextDouble < probability) {
            //flip value of a randomly selected symbol from the failed clause
            val symbol = symbols(random.nextInt(symbols.length))
            loop(counter+1, model + (symbol -> !model(symbol)))
          }
          else {
            //flip value of a symbol from failed clause, which maximizes the number
            //of satisfied clauses
            val symbol = bestSymbolToFlip(symbols, clauses, model)
            loop(counter+1,model + (symbol -> !model(symbol)))
          }
        }
      }
      else None
    }

    loop(0,randomModel)
  }

  //Returns one symbol from given symbols, flipping whose value in the model maximizes the
  //total number of satisfied clauses
  private def bestSymbolToFlip(symbols: List[PropositionSymbol], clauses: Set[Clause], model: Map[PropositionSymbol,Boolean]) = {
    //TODO: use List.max function introduced in v2.8 to do it once
    //migrated to v2.8
    def loop(symbols: List[PropositionSymbol], bestSoFar: PropositionSymbol, maxSatisfiedSoFar: Int): PropositionSymbol =
      symbols match {
        case s :: rest =>
          val n = clauses.filter(_.isTrue(model + (s -> !model(s))) == Some(true)).size
          if(n > maxSatisfiedSoFar)
            loop(rest,s,n)
          else
            loop(rest,bestSoFar,maxSatisfiedSoFar)
        case Nil => bestSoFar
      }

    loop(symbols,symbols.head,-1)
  }
}
