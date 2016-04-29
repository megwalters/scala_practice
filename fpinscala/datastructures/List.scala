package fpinscala.datastructures


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x, xs) => x + sum(xs)
	}

	def product(ds: List[Double]): Double = ds match {
		case Nil => 1.0
		case Cons(0.0, _) => 0.0
		case Cons(x, xs) => x * product(xs)
	}
	
	// Exercise 3.2
	def tail[A](tl : List[A]) : List[A] = tl match {
		case Nil => Nil 
		case Cons(x, xs) => xs
	}

	// Exercise 3.3
	def setHead[A](hl : List[A], rep: A): List[A] = hl match {
		case Nil => Nil
		case Cons(x, xs) => Cons(rep, xs)
	}

	// Exercise 3.4
	def drop[A](l: List[A], n: Int): List[A] = {
		if (n < 1) l
		else l match {
			case Nil => Nil
			case Cons(x, xs) => drop(xs, n-1)
		}	
	}
	
	// Exercise 3.5
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
		case Nil => Nil
		case Cons(x, xs) => {
			if (f(x)) dropWhile(xs, f)
			else Cons(x, xs)
		}
	}
	
	// Github solution to 3.5
	// Still does not do what I think it should do
	def dropWhileSol[A](l: List[A], f: A => Boolean): List[A] =
	    l match {
	          case Cons(h,t) if f(h) => dropWhile(t, f)
		        case _ => l
			    }

	// Exercise 3.6
	def init[A](l: List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(h, Nil) => Nil
		case Cons(h,t) => Cons(h, init(t))
	}

	//More examples from Chapter
	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
		case Nil => z
		case Cons(x, xs) => f(x, foldRight(xs, z)(f))
	}

	def sum2(ns: List[Int]) = 
		foldRight(ns, 0)((x, y) => x + y)

	def product2(ns: List[Double]) = 
		foldRight(ns, 1.0)(_ * _)

	// Exercise 3.9 
	// This is implemented without foldRight, so I could figure out how to implement length itself
	def length2[A](as: List[A]): Int = as match {
		case Nil => 0
		case Cons(h, t) => 1 + length2(t)
	}

	//Exercise 3.9 (actual exercise, implemented using FoldRight)
	def length[A](as: List[A]): Int  = {
		foldRight(as, 0)((x, y) => 1 + y)
	}

	// Exercise 3.10
	@annotation.tailrec
	def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B) : B = as match{
			case Nil => z
			case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
	}
	
	// Exercise 3.11 sum
	def sumFold(ns: List[Int]) = { 
		foldLeft(ns, 0)((x, y) => x + y)
	}

	def prodFold(ns: List[Double]) = {
		foldLeft(ns, 1.0)((x, y) => x * y)
	}

	def lengthFold[A](as: List[A]): Int = {
		foldLeft(as, 0)((x, y) => 1 + x)
	}
	
	// Exercise 3.12
	def reverse[A](as: List[A]): List[A] = {
		foldLeft(as, List[A]())((t, h) => Cons(h, t)) 
	}

	//Exercise 3.13
	def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
		foldLeft(reverse(as), z)((x,y) => f(y,x))
	}

        //Exercise 3.14 
       def append[A](as: List[A], bs:List[A]): List[A] = {
            foldRight(as, bs)((x, y) => Cons(x,y)) 
       }

        //Exercise 3.15 (Had to look up answer to this one :( )
        def concat[A](as: List[List[A]]): List[A] = 
            foldRight(as, Nil: List[A])(append)

       // def append[A](as: List[A], bs: List[A]): List[A] = as match {
        //    case Nil => bs
         //   case Cons(x, xs) => Cons(x,append(xs, bs))
        // }


	// Exercise 3.16
	def addOne(as: List[Int]): List[Int] = {
		foldRight(as, List[Int]())((a,b) => Cons(a + 1, b))
	}

	// Exercise 3.17
	def listToString(as: List[Double]): List[String] = {
		foldRight(as, List[String]())((a,b) => Cons(a.toString, b))
	}

	// Exercise 3.18
	def map[A,B](as: List[A])(f: A => B): List[B] = as match{
		case Nil => List[B]()
		case Cons(x, xs) => Cons(f(x), map(xs)(f))
		
	}

	// Exercise 3.19
	def filter[A](as: List[A])(f: A => Boolean): List[A] = {
		foldRight(as, List[A]())((a, b) => dropWhile(Cons(a, b), f))
	
	}

	// Exercise 3.20
	def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match { 
		case Nil => List[B]()
		case Cons(x, xs) => append(f(x), flatMap(xs)(f))
		
	}

	// Exercise 3.21
	def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
		flatMap(as)(a => dropWhile(Cons(a, Nil),f))
	}

	// Exercise 3.22
	def addElements(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match{
            case (Cons(x, Nil), Cons(y, Nil)) => Cons(x + y, Nil)
            case (Nil, Nil) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addElements(xs, ys))
            case (Cons(x, _), Nil) => Nil
            case (Nil, Cons(x, _)) => Nil
        
        }		
		
	// Exercise 3.23 Generalize the function you just wrote so that it's not specific to integers or addition.  Name your generalized function zipWith
        def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = (as, bs) match {
            case (Cons(x, Nil), Cons(y, Nil)) => Cons(f(x, y), Nil)
            case (Nil, Nil) => Nil
            case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
            case (Cons(x, _), Nil) => Nil
            case (Nil, Cons(x, _)) => Nil
        }

	def apply[A](as: A*): List[A] = 
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
}
