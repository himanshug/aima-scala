package aima.logic.propositional

import scala.collection.mutable.Set
import scala.collection.immutable.Map

abstract class KnowledgeBase(ss: String *) {

  protected val _sentences = Set[Sentence](ss.map(PropositionalLogicParser.parse(_)):_*)

  def tell(ss: String *) = {
    _sentences ++= ss.map(PropositionalLogicParser.parse(_))
    this
  }

  //Returns the KB as conjunction of all the sentences tell'ed
  //to the KB so far
  def sentence: Conjunction =  {
	  new Conjunction(_sentences.toList:_*)
  }

  def ask(s: String): Boolean
}

//A KnowledgeBase that inferences using TT-Entails? algorithm
class TTEntailsBasedKB(ss: String *) extends KnowledgeBase(ss:_*) {
  def ask(s: String) = TTEntails(sentence,PropositionalLogicParser.parse(s))
}

//A KnowledgeBase that inferences using PL-Resolution? algorithm
class PLResolutionBasedKB(ss: String *) extends KnowledgeBase(ss:_*) {
  def ask(s: String) = PLResolution(sentence,PropositionalLogicParser.parse(s))
}

//A KnowledgeBase that inferences using DPLL algorithm
class DPLLBasedKB(ss: String *) extends KnowledgeBase(ss:_*) {
  def ask(s: String) = {
    //if KB |= alpha then KB /\ ~alpha is unsatisfiable
    val tmp = new Negation(PropositionalLogicParser.parse(s)) :: _sentences.toList
    val kbAndNotAlpha = new Conjunction(tmp:_*)
    !DPLLSatisfiable(kbAndNotAlpha)
  }
}
