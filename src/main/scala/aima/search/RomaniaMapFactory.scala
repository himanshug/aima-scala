package aima.search

/** A factory for creating location map of Romania, as shown
 * in Fig 3.2
 *
 * @author Himanshu Gupta
 */
object RomaniaMapFactory {

  val Oradea = 'Oradea
  val Zerind = 'Zerind
  val Arad = 'Arad
  val Timisoara = 'Timisoara
  val Lugoj = 'Lugoj
  val Mehadia = 'Mehadia
  val Dobreta = 'Dobreta
  val Sibiu = 'Sibiu
  val RimnicuVilcea = 'Rimnicu_Vilcea
  val Craiova = 'Craiova
  val Fagaras = 'Fagaras
  val Pitesti = 'Pitesti
  val Bucharest = 'Bucharest
  val Giurgiu = 'Giurgiu
  val Neamt = 'Neamt
  val Iasi = 'Iasi
  val Vaslui = 'Vaslui
  val Urziceni = 'Urziceni
  val Hirsova = 'Hirsova
  val Eforie = 'Eforie
  
  
  def createRomaniaMap() = {
    val result = new LocationMap[Symbol]()

    //add various paths
    result.addPath(Oradea, Zerind, 71);
    result.addPath(Arad, Zerind, 75);
    result.addPath(Arad, Timisoara, 118);
    result.addPath(Timisoara, Lugoj, 111);
    result.addPath(Lugoj, Mehadia, 70);
    result.addPath(Mehadia, Dobreta, 75);
    result.addPath(Oradea, Sibiu, 151);
    result.addPath(Arad, Sibiu, 140);
    result.addPath(Dobreta, Craiova, 120);
    result.addPath(Sibiu, Fagaras, 99);
    result.addPath(Sibiu,RimnicuVilcea, 80);
    result.addPath(RimnicuVilcea, Craiova, 146);
    result.addPath(RimnicuVilcea, Pitesti, 97);
    result.addPath(Craiova, Pitesti, 138);
    result.addPath(Fagaras, Bucharest, 211);
    result.addPath(Pitesti, Bucharest, 101);
    result.addPath(Bucharest, Giurgiu, 90);
    result.addPath(Bucharest, Urziceni, 85);
    result.addPath(Neamt, Iasi, 87);
    result.addPath(Iasi, Vaslui, 92);
    result.addPath(Vaslui, Urziceni, 142);
    result.addPath(Urziceni, Hirsova, 98);
    result.addPath(Hirsova, Eforie, 86);

    //add straight line distances
    result.addStraightLineDistance(Bucharest,Bucharest,0);
    result.addStraightLineDistance(Bucharest,Arad,366);
    result.addStraightLineDistance(Bucharest,Craiova,160);
    result.addStraightLineDistance(Bucharest,Dobreta,242);
    result.addStraightLineDistance(Bucharest,Eforie,161);
    result.addStraightLineDistance(Bucharest,Fagaras,176);
    result.addStraightLineDistance(Bucharest,Giurgiu,77);
    result.addStraightLineDistance(Bucharest,Hirsova,151);
    result.addStraightLineDistance(Bucharest,Iasi,226);
    result.addStraightLineDistance(Bucharest,Lugoj,244);
    result.addStraightLineDistance(Bucharest,Mehadia,241);
    result.addStraightLineDistance(Bucharest,Neamt,234);
    result.addStraightLineDistance(Bucharest,Oradea,380);
    result.addStraightLineDistance(Bucharest,Pitesti,100);
    result.addStraightLineDistance(Bucharest,RimnicuVilcea,193);
    result.addStraightLineDistance(Bucharest,Sibiu,253);
    result.addStraightLineDistance(Bucharest,Timisoara,329);
    result.addStraightLineDistance(Bucharest,Urziceni,80);
    result.addStraightLineDistance(Bucharest,Vaslui,199);
    result.addStraightLineDistance(Bucharest,Zerind,374);
    result
  }
}
