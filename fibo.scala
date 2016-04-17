object fibo {
	def fib(n: Int): Int = {
		def sum(n: Int): Int = 
			if (n == 0) 0  
			else if (n == 1) 1  
			else sum(n-1) + sum(n-2)
		sum(n)
	}
	def main(args: Array[String]): Unit = 
		println(fib(5))
}
