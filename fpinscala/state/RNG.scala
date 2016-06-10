package fpinscala.state

trait RNG {
    def nextInt: (Int, RNG)
}

object RNG {

    case class Simple(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFL
	    val nextRNG = Simple(newSeed)
	    val n = (newSeed >>> 16).toInt
	    (n, nextRNG)
        }
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i1, rng1) = rng.nextInt
	if (i1 == Int.MinValue) (-i1 + 1, rng1)
	else if (i1 < 0) (-i1, rng1)
	else (i1, rng1)
    }

    def double(rng: RNG): (Double, RNG) = {
        val (i1, rng1) = rng.nextInt
        val (pos, rng2) = nonNegativeInt(rng1)
        ((pos.toDouble)/Int.MaxValue, rng2)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i1, rng1) = rng.nextInt
	val (d1, rng2) = double(rng)
	((i1, d1), rng1)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val ((i1, d1), rng1) = intDouble(rng)
	((d1, i1), rng1)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, rng1) = double(rng)
	val (d2, rng2) = double(rng1)
	val (d3, rng3) = double(rng2)
	((d1, d2, d3), rng3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def accum(n: Int, acc: List[Int])(rng_acc: RNG) : (List[Int], RNG) = {
	    if (n == 0) (acc, rng_acc)
	    else accum(n - 1, acc ++ List(rng_acc.nextInt._1))(rng_acc.nextInt._2)
	}
	accum(count, List())(rng)
    }
	    
       

}

