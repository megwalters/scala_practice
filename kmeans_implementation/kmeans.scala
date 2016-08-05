/*
* An implementation of k-means using Scala and functional programming style
*/

import util.Random.nextInt

type Point = Seq[Double]
type PointMatrix = Seq[Seq[Double]]

object LinearAlgebra {

    def subtractElements(p1: Point, p2: Point): Point = {
        p1.zip(p2).map{ case(x: Double, y: Double) => x - y }
    }

    def computeDistance(p1: Point, p2: Point): Double = {
	val differencesSquared = subtractElements(p1, p2).map(x => x*x)
	math.sqrt(sumOfElements(differencesSquared))
    }

    def sumOfElements(p1: Point): Double = {
        p1.foldLeft(0.0)((a, b) => a + b)
    }

    def meanOfElements(p1: Point): Double = {
	sumOfElements(p1) / p1.length
    }

    def transpose(pm: PointMatrix): PointMatrix = {
    	Seq.tabulate(pm(0).length, pm.length)((i,j) => pm(j)(i))
    }
}

object KMeansAlgorithm {    

    def findClosestCentroid(p: Point, cm: PointMatrix): Point = {
    	var distances = Seq[Double]()
        for (row <- cm) {
            distances = distances :+ LinearAlgebra.computeDistance(p, row)
        }
        val min_index = distances.indexOf(distances.min)
        cm(min_index)
    }

    def findMeanofCluster(pm: PointMatrix): Point = {
	val numPoints = pm.length
	val lengthPoints = pm(0).length
	// Initialize an empty list
        var accSeq = Seq[Double]()
	for (row <- LinearAlgebra.transpose(pm)) yield
            accSeq.++=(Seq(LinearAlgebra.meanOfElements(row)))
        accSeq
    }

    def computeCost(pm: PointMatrix, cm: PointMatrix): Double = {
    // pm contains all of the points, each row is a point 
    // cm contains the centroids which each point maps to
    // if the point in row 1 maps to Point c, the row 1 of cm is c
        var acc_cost = 0.0
        for (i <- 0 to pm.length - 1) 
            acc_cost = acc_cost + LinearAlgebra.computeDistance(pm(i), cm(i))
        acc_cost

    }

    def generateSeqRands(k: Int, max: Int): Seq[Int] = {
        // Generates a list of k unique random integers between 0 and max (inclusive)
        val rnd = new scala.util.Random(1000)
        var list = Seq[Int]()
        def go(cur_list: Seq[Int]): Seq[Int] = {
            var x = rnd.nextInt(max + 1)
            var temp = Seq[Int]()
            if (cur_list.contains(x)) go(cur_list) else temp = cur_list :+ x
            temp
        }
     
        while (list.length != k) list = go(list)
        list
    }

    def matchPointsToClusters(data: PointMatrix, clusters: PointMatrix): PointMatrix = {
        var matched_clusters = Seq[Seq[Double]]()
        for (row <- data) {
            var cur_centroid = findClosestCentroid(row, clusters)
            matched_clusters = matched_clusters :+ cur_centroid
        }
        matched_clusters
    }

    def updateClusters(data: PointMatrix, matched_clusters: PointMatrix)(f: PointMatrix => Point): PointMatrix = {
        val data_zipped = data.zip(matched_clusters)
        val group_map = data_zipped.groupBy(_._2)
        val new_clusters = group_map.map(x => f(x._2.map(_._1))).toSeq
        new_clusters
    } 
    
    def runKMeans(data: PointMatrix, maxIters: Int, clusters: Int): Seq[Seq[Double]] = {
        // Initiate cluster centroids
        val initial_centroid_indices = generateSeqRands(clusters, data.length)
        val initial_centroids = initial_centroid_indices.map(data)
        val initial_classification = matchPointsToClusters(data, initial_centroids)
        val initial_cost = computeCost(data, initial_classification)

        // Main loop
        def go(data: PointMatrix, centroids: PointMatrix, iter: Int, prev_cost: Double): Seq[Seq[Double]]  = {
            var new_centroids = updateClusters(data, centroids)(findMeanofCluster)
            var new_classification = matchPointsToClusters(data, new_centroids)
            var new_cost = computeCost(data, new_classification)
            if (new_cost - prev_cost != 0) {
                if (iter < maxIters) {
                    go(data, new_classification, iter + 1, new_cost)
                }
            }
            val final_indices = new_classification
            final_indices
        }
        go(data, initial_classification, 1, initial_cost)
    }
}
