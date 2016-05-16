package fpinscala.errorhandling

import scala.{Option => _, Some => _, Either => _, _}

sealed trait Either[+E, +A]{
    
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
