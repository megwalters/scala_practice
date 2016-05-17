package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Either[+E, +A]{
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
    //def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this,b) match  {
    //    case (Left(e), Left(f)) => f(this, b) 
    //    case (Left(e), Right(a)) => f(this, a)
    //    case (Right(a), Left(e)) => f(this, b)
    //    case (Right(a), Right(b)) => f(this, b)
    //} 
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
}
