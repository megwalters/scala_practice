package fpinscala.laziness

import Stream._
sealed trait Stream[+A]{
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }

    // Exercise 5.1
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => List(h()) ++ t().toList
    }

    //Exercise 5.2
    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
        case Cons(h, t) if n == 1 => cons(h(), empty) 
        case _ => empty 
    }

    //Exercise 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
        case _ => empty
    }

    def exists(p: A => Boolean): Boolean = this match {
        case Cons(h, t) => p(h()) || t().exists(p)
        case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }
    //Exercise 5.4
    def forAll(p: A => Boolean): Boolean = this match {
        case Cons(h, t) if (!p(h())) => false
        case Cons(h, t) if (p(h())) => t().forAll(p)
        case _ => true
    }

    //Exercise 5.5
    def takeWhile2(p: A => Boolean): Stream[A] = {
	foldRight(empty[A])((a, b) => if (p(a)) cons(a,b) else empty)
    }

    //Exercise 5.6
    def headOption2: Option[A] = {
	foldRight(None: Option[A])((a, b) => Some(a))
    }

    //Exercise 5.7
    //map, filter, append, and flatmap (append non-strict)
    def map[B](f: A => B): Stream[B] = {
	foldRight(empty: Stream[B])((a, b) => cons(f(a), b))
    }

    def filter(p: A => Boolean): Stream[A] = {
	foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b.takeWhile2(p)) 
    }

    def append[B>:A](z: Stream[B]): Stream[B] = {
    	foldRight(z)((a, b) => cons(a, b))
	}
	
	def flatMap[B](f: A => Stream[B]): Stream[B] = {
		foldRight(empty: Stream[B])((a, b) => f(a).append(b))
	}
	

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = t1
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = 
        if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    
}
