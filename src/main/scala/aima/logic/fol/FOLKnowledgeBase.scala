package aima.logic.fol

/** FOL KnowledgeBase representation.
 *
 * @author Himanshu Gupta
 */
class FOLKnowledgeBase {

  //sentences told so far in original form
  private var _originalSentences = Set[String]()

  //sentences in parsed Sentence form
  private var _sentences = Set[Sentence]()

  //sentences in CNF form
  private var _clauses = Set[Clause]()

  //all definite clauses
  private var _definiteClauses = Set[FOLDefiniteClause]()

  //all Implication Definite Clauses
  private var _implicationDefiniteClauses = Set[ImplicationDefiniteClause]()

  //simple definite clauses
  private var _atomicSentences = Set[AtomicSentence]()

  //all predicates
  private var _predicates = Map[String,Set[Predicate]]()

  //all equals
  private var _equals = Set[Equal]()


  //Getters
  def definiteClauses = _definiteClauses
  def implicationDefiniteClauses = _implicationDefiniteClauses
  def clauses = _clauses


  //TELL sentence to this KB
  def tell(s: String) = {
    _originalSentences = _originalSentences + s
    store(FOLParser.parse(s))
    this
  }
  def tell(s: Sentence) = { store(s); this }
  def tell(ss: Set[AtomicSentence]) = { ss.foreach(store(_)); this }


  //STORE - described in 1st paragraph, section 9.2.3
  //
  //Stores a sentence in KB
  def store(sentence: Sentence) {
    //TODO: should we check if the sentence already in KB
    _sentences = _sentences + sentence

    //convert to CNF form
    val newClauses = SentenceToCNF(sentence,this)
    _clauses = _clauses ++ newClauses

    //filter out the definite clauses from newClauses
    val newDefiniteClauses = newClauses.filter(x => x.isDefiniteClause).map(_.toDefiniteClause)
    _definiteClauses = _definiteClauses ++ newDefiniteClauses

    //separate the implication definite clauses and simple one(atomic sentence)
    val newImplications = newDefiniteClauses.filter(_ match {
      case _: ImplicationDefiniteClause => true
      case _: AtomicSentence => false
    }).map(_.asInstanceOf[ImplicationDefiniteClause])
    _implicationDefiniteClauses = _implicationDefiniteClauses ++ newImplications

    val newAtoms = newDefiniteClauses.filter(_ match {
      case _: ImplicationDefiniteClause => false
      case _: AtomicSentence => true
    }).map(_.asInstanceOf[AtomicSentence])
    _atomicSentences = _atomicSentences ++ newAtoms

    //index the atomic sentences
    newAtoms.foreach(indexAtomicSentence(_))
  }

  //index the atomic sentence
  def indexAtomicSentence(s: AtomicSentence) =
    s match {
      case x: Predicate =>
        _predicates.get(x.symbol) match {
          case None => _predicates = _predicates + (x.symbol -> Set(x))
          case Some(ps) =>
            _predicates = _predicates + (x.symbol -> (ps + x))
        }
      case x: Equal =>
        _equals = _equals + x
    }


  //FETCH - described in 1st paragraph, section 9.2.3
  //
  //Returns list of all unifiers that unifies input sentence
  //with some sentence(s) in the KB
  def fetch(s: AtomicSentence): Set[Map[Variable,Term]] = {
    s match {
      case x: Predicate =>
        _predicates.get(x.symbol) match {
          case None => Set[Map[Variable,Term]]()
          case Some(ps) => fetch(x,ps)
        }
      case x: Equal => fetch(x,_equals)
    }
  }

  //ss are conjuncts of some sentence 
  def fetch(ss: Set[AtomicSentence]): Set[Map[Variable,Term]] = {
      
    def converge(s1: Set[Map[Variable,Term]], s2: Set[Map[Variable,Term]]) =
      s1.flatMap(m =>
        s2.map(Unify.merge(_,m)).filter(_ != None)).map(_.get)

    if (ss.isEmpty) Set[Map[Variable,Term]]()
    else
      ss.map(fetch(_)).reduceLeft(converge(_,_))
  }

  //def fetch(l: Literal): Set[Map[Variable,Term]] = fetch(l.sentence)

  //Returns set of unifiers that unifies s with elements in set
  def fetch[T](s: T, set: Set[T]): Set[Map[Variable,Term]] =
    set.map(Unify(s,_) match {
              case None => null
              case Some(x) => x
            }).filter(_ != null)

  

  //Generators for unique(for this KB) Constant, 
  //Variable and Function
  private var seqConstant = 0
  private var seqVariable = 0
  private var seqFunction = 0

  def generateConstant =
    synchronized {
      seqConstant = seqConstant + 1
      Constant("C$$" + seqConstant)
    }

  def generateVariable: Variable = generateVariable("")
  def generateVariable(prefix: String) =
    synchronized {
      seqVariable = seqVariable + 1
      Variable(prefix + "$$" + seqVariable)
    }

  def generateFunction(args: List[Term]) =
    synchronized {
      seqFunction = seqFunction + 1
      new Function("F$$" + seqFunction, args:_*)
    }
}
