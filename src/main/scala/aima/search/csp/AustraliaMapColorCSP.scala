package aima.search.csp

/** Factory for Australia Map color CSP
 * described in Fig 6.1
 *
 * @author Himanshu Gupta
 */
object AustraliaMapColorCSP {
  val Red = 0
  val Green = 1
  val Blue = 2

  private val domain = List(Red,Green,Blue)

  val Wa = "WA"
  val Nt = "NT"
  val Q = "Q"
  val Sa = "SA"
  val Nsw = "NSW"
  val V = "V"
  val T = "T"

  def csp = {
    
    val prob = new CSP[String,Int]()

    prob.addVariables((Wa,domain),
                      (Nt,domain),
                      (Q,domain),
                      (Sa,domain),
                      (Nsw,domain),
                      (V,domain),
                      (T,domain))                      
    prob.addConstraints(new AllDiffConstraint(Wa,Nt),
                       new AllDiffConstraint(Nt, Sa),
                       new AllDiffConstraint(Wa, Sa),
                       new AllDiffConstraint(Nt, Q),
                       new AllDiffConstraint(Q, Sa),
                       new AllDiffConstraint(Nsw, Sa),
                       new AllDiffConstraint(Q, Nsw),
                       new AllDiffConstraint(V, Sa),
                       new AllDiffConstraint(V, Nsw))
    prob }
}
