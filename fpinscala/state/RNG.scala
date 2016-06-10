//package fpinscala.state

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
    
    // Exercise 6.1
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (i1, rng1) = rng.nextInt
	if (i1 == Int.MinValue) (-i1 + 1, rng1)
	else if (i1 < 0) (-i1, rng1)
	else (i1, rng1)
    }
    
    // Exercise 6.2
    def double(rng: RNG): (Double, RNG) = {
        val (i1, rng1) = rng.nextInt
        val (pos, rng2) = nonNegativeInt(rng1)
        ((pos.toDouble)/Int.MaxValue, rng2)
    }

    // Exercise 6.3
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
    
    // Exercise 6.4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def accum(n: Int, acc: List[Int])(rng_acc: RNG) : (List[Int], RNG) = {
	    if (n == 0) (acc, rng_acc)
	    else accum(n - 1, acc ++ List(rng_acc.nextInt._1))(rng_acc.nextInt._2)
	}
	accum(count, List())(rng)
    }

    type Rand[+A] = RNG => (A, RNG)

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = 
        rng => (a, rng)
    
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }
    
    def nonNegativeEven: Rand[Int] = 
        map(nonNegativeInt)(i => i - i % 2)
    
    // Exercise 6.5
    def double2: Rand[Double] = 
        map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

    // Exercise 6.6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        rng => {
            val (a, rng2) = ra(rng)
            val (b, rng3) = rb(rng)
            (f(a,b), rng3)
        }
    }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
        map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] = 
        both(int, double)

    val randDoubleInt: Rand[(Double, Int)] = 
        both(double, int)

    // Exercise 6.7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
        fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_::_))
    }

    // Come back to this - are there supposed to be different numbers in here?
    def ints2(count: Int): Rand[List[Int]] = {
        val list = List.fill(count)(int)
        sequence(list)
    }

    // Exercise 6.8
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
        rng => {
            val (a, rng2) = f(rng)
            g(a)(rng2)
        }
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
        flatMap(nonNegativeInt){x => 
            val mod = x % n
            if (x + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
        }
    }
    
    // Exercise 6.9
    def mapWithFlat[A, B](s: Rand[A])(f: A => B): Rand[B] = 
        flatMap(s)(x => unit(f(x)))

    def map2WithFlat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
        flatMap(ra)(a => map(rb)(b => f(a, b)))
    }
    
    
}

