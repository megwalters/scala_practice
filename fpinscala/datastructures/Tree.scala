package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // Exercise 3.25
    def size[A](t: Tree[A]): Int = t match{
        case Leaf(_) => 1
        case Branch(l,r) => 1 + size(l) + size(r)
    }
   
   // Exercise 3.26 
   def max(t: Tree[Int]): Int = t match {
       case Leaf(x) => x
       case Branch(Leaf(y), Leaf(x)) => x max y
       case Branch(l, r) => max(l) max max(r)
    }  

    // Exercise 3.27
    def maxLength[A](t: Tree[A]): Int = t match {
        case Leaf(x) => 1
        case Branch(l, r) => 1 + maxLength(l) max maxLength(r)
    }
   
   // Exercise 3.28 
    def map[A](t: Tree[A], f: A => A): Tree[A] = t match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(map(l, f), map(r, f))
    } 

    // Exercise 3.29
    def fold[A,B](t: Tree[A])(f:A => B)(g: (B, B) => B): B = t match {
        case Leaf(x) => f(x)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size2[A](t: Tree[A]): Int = {
        fold(t)(x => 1)((x, y) => 1 + x + y)
    }
} 
