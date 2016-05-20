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
