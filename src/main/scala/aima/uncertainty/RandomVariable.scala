package aima.uncertainty

/** RandomVariable, as described in Ch. 13
 *
 * @author Himanshu Gupta
 */
class RandomVariable private(val name: String, val domain: Set[String]) {

  override def equals(that: Any) =
    that match {
      case x: RandomVariable => x.name == this.name
      case _ => false
    }

  override def hashCode = name.hashCode

  override def toString = name
}

//RandomVariable Factory
object RandomVariable {

  val True = "true"
  val False = "false"

  private val booleanDomain = Set(True,False)

  //Returns a RandomVariable with Boolean domain
  def apply(name: String) = new RandomVariable(name,booleanDomain)

  def apply(name: String, domain: Set[String]) = new RandomVariable(name,domain)
}

