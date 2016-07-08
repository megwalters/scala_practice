/*
* An implementation of k-means using Scala and functional programming style
*/

import util.Random.nextInt

object KMeansAlgorithm {
    
    type Point = List[Double]
    type PointMatrix = List[List[Double]]
    
    def subtractElements(p1: Point, p2: Point): Point = (p1, p2) match {
    	case (_, Nil) => Nil
	case (Nil, _) => Nil
	case (h1::t1, h2::t2) => (h1 - h2)::subtractElements(t1, t2)
    }

    def computeDistance(p1: Point, p2: Point): Double = {
        val differences = subtractElements(p1, p2)
	val differencesSquared = differences.map(x => x*x)
	math.sqrt(sumOfElements(differencesSquared))
    }

    def sumOfElements(p1: Point): Double = p1 match {
    	case Nil => 0
	case h1::t1 => h1 + sumOfElements(t1)
    }

    def findClosestCentroid(p: Point, cm: PointMatrix): Point = {
    	def CurrMin(p1: Point, p2: Point, min: Double): Boolean = {
	    val distance = computeDistance(p1, p2)
	    if (distance < min) true
	    else false
	}
	
	var curMin = (p, Double.MaxValue)
	for (row <- cm) yield
		if (CurrMin(p, row, curMin._2)) curMin = (row, computeDistance(p, row))
	
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

    def findMeanofCluster(pm: PointMatrix): Point = {
	val numPoints = pm.length
	val lengthPoints = pm(0).length

	// Initialize an empty list
        var accList = List[Double]()
	for (row <- transpose(pm)) yield
            accList.++=(List(meanOfElements(row)))
        accList
    }

    def computeCost(pm: PointMatrix, cm: PointMatrix): Double = {
    // pm contains all of the points, each row is a point 
    // cm contains the centroids which each point maps to
    // if the point in row 1 maps to Point c, the row 1 of cm is c
        var acc_cost = 0.0
        for (i <- 0 to pm.length - 1) 
            acc_cost = acc_cost + computeDistance(pm(i), cm(i))
        acc_cost

    }

    def generateListRands(k: Int, max: Int): List[Int] = {
        // Generates a list of k unique random integers between 0 and max (inclusive)
        val rnd = new scala.util.Random(1000)
        var list = List[Int]()
        def go(cur_list: List[Int]): List[Int] = {
            var x = rnd.nextInt(max + 1)
            var temp = List[Int]()
            if (cur_list.contains(x)) go(cur_list) else temp = x :: cur_list 
            temp
        }
     
        while (list.length != k) list = go(list)
        list
    }

    //def runKMeans(data: PointMatrix, maxIters: Int = 1000, threshold: Double = .001, clusters: Int = 10)): (List[Double], PointMatrix) = {
        // Choose k random columns from data to use as initial centroids
         



}
