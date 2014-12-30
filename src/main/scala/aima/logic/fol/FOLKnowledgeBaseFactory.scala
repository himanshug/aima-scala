package aima.logic.fol

/** Factory for creating various example FOL KB
 * discussed in the book
 *
 * @author Himanshu Gupta
 */
object KBFactory {

  //KB descrived in Section 9.1.2
  def kingsKB = {
    val kb = new FOLKnowledgeBase
    kb.tell("((King(x) & Greedy(x)) => Evil(x))")
    kb.tell("King(John)");
    kb.tell("King(Richard)");
    kb.tell("Greedy(John)");
    kb
  }

  //KB described in Section 9.3.1
  def weaponsKB = {
    val kb = new FOLKnowledgeBase
    kb.tell("American(x) & Weapon(y) & Sells(x,y,z) & Hostile(z) => Criminal(x)")
    kb.tell("Owns(Nono, M1)")
    kb.tell("Missile(M1)")
    kb.tell("Missile(x) & Owns(Nono,x) => Sells(West,x,Nono)")
    kb.tell("Missile(x) => Weapon(x)")
    kb.tell("Enemy(x,America) => Hostile(x)")
    kb.tell("American(West)")
    kb.tell("Enemy(Nono,America)")
    kb
  }

  //KB described in Section 9.5.3
  def lovesAnimalKB = {
    val kb = new FOLKnowledgeBase
    kb.tell("4L x (4L y Animal(y) => Loves(x,y)) => (3E y Loves(y,x))")
    kb.tell("4L x (3E z Animal(z) & Kills(x,z)) => (4L y ~Loves(y,x))")
    kb.tell("4L x Animal(x) => Loves(Jack,x)")
    kb.tell("Kills(Jack,Tuna) | Kills(Curiosity,Tuna)")
    kb.tell("Cat(Tuna)")
    kb.tell("4L x Cat(x) => Animal(x)")
    kb
  }

  // Note: see -
  // http://logic.stanford.edu/classes/cs157/2008/lectures/lecture15.pdf
  // slide 12 for where this test example was taken from.
  def aBCEqualityKB(includeEqualityAxioms: Boolean) = {
    val kb = new FOLKnowledgeBase
    kb.tell("B = A");
    kb.tell("B = C");

    if (includeEqualityAxioms) {
      kb.tell("x = x")                        // Reflexivity Axiom
      kb.tell("(x = y => y = x)")             // Symmetry Axiom
      kb.tell("((x = y & y = z) => x = z)")   // Transitivity Axiom
    }
    kb
  }
}
