package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Either[+E, +A]{
    //Exercise 4.6
    def map[B](f: A => B): Either[E,B] = this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => f(a) 
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => b
        case Right(a) => Right(a)
    }  
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this,b) match  {
    	case (Left(e1), Left(e2)) => Left(e1)
    	case (Left(e), Right(a)) => Left(e) 
    	case (Right(a), Left(e)) => Left(e)
    	case (Right(a), Right(b)) => Right(f(a,b)) 
    }

    
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
    def mean(xs: Seq[Double]): Either[String, Double] = {
        if (xs.isEmpty) 
            Left("mean of empty list!")
        else
            Right(xs.sum/xs.length)
    }
    
    def Try[A](a: => A): Either[Exception, A] = {
        try Right(a)
        catch {case e: Exception => Left(e)}
    }  
	
	//Exercise 4.7
    def sequence[E, A](es: List[Either[E, A]]) : Either[E, List[A]] = es match {
    	case Nil => Right(Nil)
	case eh::et => eh.flatMap(h => sequence(et).map(h::_))
}
	

	def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
		case Nil => Right(Nil)
		case ah::at => f(ah).map2(traverse(at)(f))(_::_)
	}
} 
