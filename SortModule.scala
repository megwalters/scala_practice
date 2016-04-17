object SortModule {
	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		def compare(n: Int): Boolean = 
			if (n >= as.length) true
			else if (ordered(as(n), as(n-1))) false
			else compare(n + 1)
		compare(1)
	}

	private def intCompare(n: Int, m: Int): Boolean = {
		if (n <= m) true
		else false
	}

	def main(args: Array[String]): Unit =
		println(isSorted(Array(1,2,4,3), intCompare))
}
	
