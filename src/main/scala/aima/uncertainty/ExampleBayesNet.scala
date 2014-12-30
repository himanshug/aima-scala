package aima.uncertainty

/** Some example Bayes Networks
 *
 * @author Himanshu Gupta
 */
object ExampleBayesNet {


  val True = RandomVariable.True
  val False = RandomVariable.False


  /** Bayes Net for the Burglary network described in Fig 14.2 */
  def burglaryNetwork = {
    val burglary = RandomVariable("Burglary")
    val earthQuake = RandomVariable("EarthQuake")
    val johnCalls = RandomVariable("JohnCalls")
    val alarm = RandomVariable("Alarm")
    val maryCalls = RandomVariable("MaryCalls")

    new BayesNet()
    .add(burglary,Set.empty,
         Map(Map((burglary,True)) -> 0.001,
             Map((burglary,False)) -> 0.999))
    .add(earthQuake,Set.empty,
         Map(Map((earthQuake,True)) -> 0.002,
             Map((earthQuake,False)) -> 0.998))
    .add(alarm,Set(burglary,earthQuake),
         Map(Map((burglary,True),(earthQuake,True),(alarm,True)) -> 0.95,
             Map((burglary,True),(earthQuake,False),(alarm,True)) -> 0.94,
             Map((burglary,False),(earthQuake,True),(alarm,True)) -> 0.29,
             Map((burglary,False),(earthQuake,False),(alarm,True)) -> 0.001,
             Map((burglary,True),(earthQuake,True),(alarm,False)) -> 0.05,
             Map((burglary,True),(earthQuake,False),(alarm,False)) -> 0.06,
             Map((burglary,False),(earthQuake,True),(alarm,False)) -> 0.71,
             Map((burglary,False),(earthQuake,False),(alarm,False)) -> 0.999))
    .add(johnCalls,Set(alarm),
         Map(Map((alarm,True),(johnCalls,True)) -> 0.9,
             Map((alarm,False),(johnCalls,True)) -> 0.05,
             Map((alarm,True),(johnCalls,False)) -> 0.1,
             Map((alarm,False),(johnCalls,False)) -> 0.95))
    .add(maryCalls,Set(alarm),
         Map(Map((alarm,True),(maryCalls,True)) -> 0.7,
             Map((alarm,False),(maryCalls,True)) -> 0.01,
             Map((alarm,True),(maryCalls,False)) -> 0.3,
             Map((alarm,False),(maryCalls,False)) -> 0.99))
  }

  /** Bayes Net for the Cloudy network described in Fig 14.12 */
  def cloudyNetwork = {
    val cloudy = RandomVariable("Cloudy")
    val sprinkler = RandomVariable("Sprinkler")
    val rain = RandomVariable("Rain")
    val wetGrass = RandomVariable("WetGrass")

    new BayesNet()
    .add(cloudy,Set.empty,
         Map(Map((cloudy,True)) -> 0.5,
             Map((cloudy,False)) -> 0.5))
    .add(sprinkler,Set(cloudy),
         Map(Map((cloudy,True),(sprinkler,True)) -> 0.1,
             Map((cloudy,False),(sprinkler,True)) -> 0.5,
             Map((cloudy,True),(sprinkler,False)) -> 0.9,
             Map((cloudy,False),(sprinkler,False)) -> 0.5))
    .add(rain,Set(cloudy),
         Map(Map((cloudy,True),(rain,True)) -> 0.8,
             Map((cloudy,False),(rain,True)) -> 0.2,
             Map((cloudy,True),(rain,False)) -> 0.2,
             Map((cloudy,False),(rain,False)) -> 0.8))
    .add(wetGrass,Set(sprinkler,rain),
         Map(Map((sprinkler,True),(rain,True),(wetGrass,True)) -> 0.99,
             Map((sprinkler,True),(rain,False),(wetGrass,True)) -> 0.90,
             Map((sprinkler,False),(rain,True),(wetGrass,True)) -> 0.90,
             Map((sprinkler,False),(rain,False),(wetGrass,True)) -> 0.00,
             Map((sprinkler,True),(rain,True),(wetGrass,False)) -> 0.01,
             Map((sprinkler,True),(rain,False),(wetGrass,False)) -> 0.10,
             Map((sprinkler,False),(rain,True),(wetGrass,False)) -> 0.10,
             Map((sprinkler,False),(rain,False),(wetGrass,False)) -> 1.0))
  }          
}
