object function_composition {
	def compose[A,B,C](f: B => C, g: A => B): A => C = {
		(a: A) => f(g(a))
	}

	def square(x: Int): Int = {
		x*x
	}

	def add(x: Int): Int = {
		x + 1
	}

	def main(args: Array[String]): Unit = {
		println(compose(add, square)(2)) 
	}
}
