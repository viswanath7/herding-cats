package com.example.cats.foldable

import cats.Eval
import cats.Foldable
import cats.instances.stream._

object FoldableDemo extends App {

	val numbers = (1 to 1000000).toStream
	
	def foldLeftSummation =
		Foldable[Stream].foldLeft(numbers, 0)(_ + _)
	
	println(s"Fold left summation: $foldLeftSummation")
	
	def foldRightSummation =
		Foldable[Stream].foldRight(numbers, Eval.now(0L)) { (num, eval) => eval.map(_+num) }
	
	println(s"Fold left summation: ${foldRightSummation.value}")
	
}
