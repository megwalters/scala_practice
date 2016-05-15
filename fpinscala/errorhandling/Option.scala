package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A]{

	//Exercise 4.1
	def map[B](f: A => B): Option[B] = this match{
		case None => None
		case Some(a) => Some(f(a))
	}

	def flatMap[B](f: A => Option[B]): Option[B] = {
		map(f).getOrElse(None)	
	}

	def getOrElse[B >: A](default: => B): B = this match{
		case None => default
		case Some(a) => a
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
		case None => ob
		case Some(a) => Some(a)
	}

	def filter(f: A => Boolean): Option[A] = this match{
		case None => None
		case Some(a) => if (f(a)) Some(a) else None
	}




}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option{
	def mean(xs: Seq[Double]): Option[Double] = {
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}

	// Exercise 4.2
	def variance(xs: Seq[Double]): Option[Double] = {
		mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
	}

	//Exercise 4.3 
	def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match{
		case (None, None) => None
		case (None, Some(b)) => None
		case (Some(a), None) => None
		case (Some(a), Some(b)) => Some(f(a, b))
	}

	//def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
	//	case Nil => None
	//	case None::at => None
	//	case at::None => None 
	//	case _::at => sequence(at.getOrElse(None))
	//}		

}

