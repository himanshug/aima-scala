package aima.uncertainty

import junit.framework._
import Assert._

class BayesNetTest extends TestCase {

  private val True = RandomVariable.True
  private val False = RandomVariable.False

  def testGetMarkovBlanketProbabilityDistribution() {
    val p = ExampleBayesNet.cloudyNetwork.
      getMarkovBlanketProbabilityDistribution(RandomVariable("Sprinkler"),
                                              Map(RandomVariable("Cloudy") -> True,
                                                  RandomVariable("Rain") -> True,
                                                  RandomVariable("WetGrass") -> True))
    assertEquals(0.099,p(True),0.001)
    assertEquals(0.81,p(False),0.001)
  }
}
