/*
* An implementation of k-means using Scala and functional programming style
*/

object KMeansAlgorithm {
    
    type Point = List[Double]
    type PointMatrix = List[List[Double]]
    
    def subtractElements(p1: Point, p2: Point): Point = (p1, p2) match {
    	case (_, Nil) => Nil
	case (Nil, _) => Nil
	case (h1::t1, h2::t2) => (h1 - h2)::subtractElements(t1, t2)
    }

    def computeDistanceSquared(p1: Point, p2: Point): Double = {
        val differences = subtractElements(p1, p2)
	val differencesSquared = differences.map(x => x*x)
	sumOfElements(differencesSquared)
    }

    def sumOfElements(p1: Point): Double = p1 match {
    	case Nil => 0
	case h1::t1 => h1 + sumOfElements(t1)
    }

    def findClosestCentroid(p: Point, cm: PointMatrix): Point = {
    	def CurrMin(p1: Point, p2: Point, min: Double): Boolean = {
	    val distance = computeDistanceSquared(p1, p2)
	    if (distance < min) true
	    else false
	}
	
	var curMin = (p, Double.MaxValue)
	for (row <- cm) yield
		if (CurrMin(p, row, curMin._2)) curMin = (row, computeDistanceSquared(p, row))
	
	curMin._1
	
    }

    def meanOfElements(p1: Point): Double = {
    	val sum = sumOfElements(p1)
	val numElements = p1.length
	sum / numElements
    }

    def transpose(pm: PointMatrix): PointMatrix = {
    	List.tabulate(pm(0).length, pm.length)((i,j) => pm(j)(i))
    }

    //Broken right now!
    def findMeanofCluster(pm: PointMatrix): Point = {
	val numPoints = pm.length
	val lengthPoints = pm(0).length

	// Initialize an empty list
        var accList = List()
	for (col <- transpose(pm)) yield
		accList = accList :+ meanOfElements(col)
	accList
    }
}
