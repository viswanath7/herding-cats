package com.example.cats.foldable

object FoldListLeftRightDemo extends App {
	
	val foldLeftResult = (1 to 5).toList
		.foldLeft(List.empty[Int])( (accumulator, num) => num::accumulator)
	
	println(s"List fold left: $foldLeftResult")
	
	val foldRightResult = (1 to 5).toList
		.foldRight(List.empty[Int])( (num, accumulator) => num::accumulator)
	
	println(s"List fold right: $foldRightResult")
	
}
