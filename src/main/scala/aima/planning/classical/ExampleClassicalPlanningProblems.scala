package aima.planning.classical


object ClassicalPlanningProblems {

  /** The "have cake and eat case too" problem,
   * described in Fig 10.7
   *
   * @author Himanshu Gupta
   */
  def haveCakeAndEatCakeToo: ClassicalPlanningProblem =
    haveCakeAndEatCakeToo("Have(Cake) & Eaten(Cake)")

  def haveCakeAndEatCakeToo(goals: String): ClassicalPlanningProblem =
    ClassicalPlanningProblem(
      "Have(Cake)",
      goals,
      Action(
        "Eat(Cake)",
        "Have(Cake)",
        "~Have(Cake) & Eaten(Cake)"
      ),
      Action(
        "Bake(Cake)",
        "~Have(Cake)",
        "Have(Cake)"
      ))

  /** The "spare tire" problem, described in
   * Fig 10.2
   * This is a *variable free* propositional
   * version of that
   * 
   * @author Himanshu Gupta
   */
  def spareTire: ClassicalPlanningProblem = spareTire("At(Spare,Axle)")

  def spareTire(goals: String): ClassicalPlanningProblem =
    ClassicalPlanningProblem(
      "At(Flat,Axle) & At(Spare,Trunk)",
      goals,
      Action(
        "Remove(Flat,Axle)",
        "At(Flat,Axle)",
        "~At(Flat,Axle) & At(Flat,Ground)"
      ),
      Action(
        "Remove(Spare,Axle)",
        "At(Spare,Axle)",
        "~At(Spare,Axle) & At(Spare,Ground)"
      ),
      Action(
        "Remove(Flat,Trunk)",
        "At(Flat,Trunk)",
        "~At(Flat,Trunk) & At(Flat,Ground)"
      ),
      Action(
        "Remove(Spare,Trunk)",
        "At(Spare,Trunk)",
        "~At(Spare,Trunk) & At(Spare,Ground)"
      ),
      Action(
        "PutOn(Flat,Axle)",
        "At(Flat,Ground) & ~At(Flat,Axle) & ~At(Spare,Axle)",
        "At(Flat,Axle) & ~At(Flat,Ground)"
      ),
      Action(
        "PutOn(Spare,Axle)",
        "At(Spare,Ground) & ~At(Spare,Axle) & ~At(Flat,Axle)",
        "At(Spare,Axle) & ~At(Spare,Ground)"
      ))
}
        
