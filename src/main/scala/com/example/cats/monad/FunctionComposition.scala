package com.example.cats.monad

import cats.instances.function._
import cats.syntax.functor._

object FunctionComposition extends App {
	
	val firstFunction: Int => Double =
		(input: Int) => input.toDouble
	
	val secondFunction: Double => Double =
		(input: Double) => input * 2
	
	 /*
	 Note: Function1 does not have 'map' defined.
	 It works here because the type class instance for Function1 from cats provides it
	 Here we employ the interface method syntax to chain function calls
	 */
	val chainedFinalFunction = firstFunction map secondFunction
	
	println(s"Result of function composition using map: ${chainedFinalFunction(1)}")
}
