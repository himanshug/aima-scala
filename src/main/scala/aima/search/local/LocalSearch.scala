package aima.search.local

import aima.search.{Problem}

/** Hill-Climbing-Search, described in Fig 4.2
 *
 * @author Himanshu Gupta
 */
object HillClimbingSearch {
  def apply[S,A](problem: Problem[S,A], value: (S)=>Double) = {

    def getHighestValuedSuccessor(s: S): Option[(Double,S)] = {
      val successors = problem.actions(s).map(problem.result(s,_))
      if(!successors.isEmpty) {
        val tmp = successors.map((st) => (value(st),st))
        Some(tmp.foldLeft(tmp.head)((a,b)=>if(a._1 < b._1) b else a))
      }
      else None
    }
    def loop(current: S): S = {
      getHighestValuedSuccessor(current) match {
        case Some((v,n)) =>
          if(v <= value(current)) current
          else loop(n)
        case None => current
      }
    }
    loop(problem.initialState)
  }
}

/** Simulated-Annealing-Search, described in Fig 4.5
 *
 * @author Himanshu Gupta
 */
object SimulatedAnnealingSearch {
  def apply[S,A](problem: Problem[S,A],value: (S)=>Double, schedule: (Int)=>Double) = {

    val random = new scala.util.Random(new java.util.Random)

    def randomSuccessor(s: S): Option[S] = {
      val successors = problem.actions(s).map(problem.result(s,_))
      if(successors.isEmpty) None
      else {
        Some(successors(random.nextInt(successors.length)))
      }
    }
 
    def loop(current: S, t: Int): S = {
      val T = schedule(t)
      if(T == 0.0) current
      else {
        randomSuccessor(current) match {
          case Some(next) => {
            val dE = value(next) - value(current)
            if(dE > 0.0) loop(next,t+1)
            else {
              if(Math.random < Math.exp(dE/T)) loop(next,t+1)
              else loop(current,t+1)
            }}
          case None => current
        }
      }
    }
    loop(problem.initialState,1)
  }
}

/** Genetic-Algorithm, described in Fig 4.8
 *
 * @author Himanshu Gupta
 */
class GeneticAlgorithm(finiteAlphabet: Array[Char], mutationProbability: Double) {

  private val random = new scala.util.Random(new java.util.Random)
  

  def geneticAlgorithm(population: List[String], fitnessFn: (String)=>Double, goalTest: (String)=>Boolean, maxIterations: Int) = {

    def loop(population: List[String], i: Int): String = {
      val bestMember = retrieveBestIndividual(population,fitnessFn)
      if(i == maxIterations || goalTest(bestMember))
        bestMember
      else {
        var newPopulation: List[String] = Nil
        for(_ <- 1 to population.size) {
          val x = randomSelect(population,fitnessFn)
          val y = randomSelect(population,fitnessFn)
          val child = reproduce(x,y)
          if(Math.random < mutationProbability) newPopulation = mutate(child) :: newPopulation
          else newPopulation = child :: newPopulation
        }
        loop(newPopulation,i+1)
      }
    }
    loop(population,0)
  }

  private def reproduce(m1: String, m2: String) = {
    val len = m1.length
    val n = random.nextInt(len)
    m1.substring(0,n)+m2.substring(n,len)
  }

  private def mutate(m: String) = {
    val tmp = new StringBuilder(m)
    tmp.setCharAt(random.nextInt(m.length),finiteAlphabet(random.nextInt(finiteAlphabet.length)))
    tmp.toString
  }

  private def randomSelect(population: List[String],fitnessFn: (String)=>Double) = {
    val fVals = population.map(fitnessFn)
    val totalF = fVals.foldLeft(0.0)(_+_)
    val normalizedFvals = fVals.map(_/totalF)
    val prob = Math.random

    def loop(population: List[String],normalizedFvals: List[Double],totalSoFar: Double): String =
      (population,normalizedFvals) match {
        case (p :: Nil,_) => p
        case (p :: ps, n :: ns) => if(prob <= totalSoFar) p else loop(ps,ns,n+totalSoFar)
        case (Nil,_) => throw new IllegalStateException("failed to find random member.")
      }
    loop(population,normalizedFvals,normalizedFvals.head)
  }

  private def retrieveBestIndividual(population: List[String],fitnessFn: (String)=>Double) = {
    def loop(population: List[String], best: String, bestFval: Double): String =
      population match {
        case p :: Nil => {
          if(fitnessFn(p) > bestFval) p
          else best }
        case p :: rest => {
          val fval = fitnessFn(p)
          if(fval > bestFval) loop(rest,p,fval)
          else loop(rest,best,bestFval) }
        case Nil => throw new IllegalStateException("failed to find best individual.")
      }
    loop(population,null,Math.MIN_DOUBLE)
  }
}
