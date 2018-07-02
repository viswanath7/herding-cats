package com.example.cats.foldable

object FoldDemo extends App {
	
	def massiveData = (1 to 100000).toStream
	
	def foldRightSummation(items: Stream[Int]) =
		items.foldRight(0L)((accumulator, item) => accumulator+item )
	
	def foldLeftSummation(items: Stream[Int]) =
		items.foldLeft(0L)((accumulator, item) => accumulator+item)
	
	// Following should work
	println(s"Fold left summation: ${foldLeftSummation(massiveData)}")
	
	// Following should fail fatally with stack overflow error!
	println(s"Fold right summation: ${foldRightSummation(massiveData)}")
}
