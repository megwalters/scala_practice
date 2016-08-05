/* Creates a randomly generated data set */

import util.Random.nextFloat

object DataGenerator {

    val num_points = 100
    val dimension = 2

    var data = Seq[Seq[Double]]()

    def createNewPoint(rng: scala.util.Random, dim: Int): Seq[Double] = { 
        Seq.fill(dim)(rng.nextFloat)
    }

    def addNewPoint(data: Seq[Seq[Double]], rng: scala.util.Random, dim: Int): Seq[Seq[Double]] = {
        val new_point = createNewPoint(rng, dim)
        data :+ new_point
    }

    def createData(num_points: Int, dimension: Int, rng: scala.util.Random): Seq[Seq[Double]] = {
        def go(rng: scala.util.Random, dimension: Int, so_far: Seq[Seq[Double]], points_added: Int): Seq[Seq[Double]] = {
            var curr_seq = addNewPoint(so_far, rng, dimension)
            if (points_added < num_points - 1) {
                go(rng, dimension, curr_seq, points_added + 1)
            curr_seq   
        }

        go(rng, dimension, Seq[Seq[Double]](), 0)
    }
       
}





