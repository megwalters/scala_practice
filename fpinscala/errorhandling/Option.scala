package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A]{
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

	//TODO This still doesn't work
	def variance(xs: Seq[Double]): Option[Double] = {
		mean(xs).flatMap((xs: Seq[Double]) => mean(xs.map((x: Double) => math.pow(x - mean(xs), 2))))
	}

}

