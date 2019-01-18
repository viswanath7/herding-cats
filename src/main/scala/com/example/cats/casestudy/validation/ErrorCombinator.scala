package com.example.cats.casestudy.validation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated._
import cats.syntax.semigroup._
import cats.syntax.apply._
import cats.syntax.semigroup._ // for |+|

object ErrorCombinator extends App {
	
	import cats.data.NonEmptyList
	
	type ErrorMessages = NonEmptyList[String]
	
	def checkMinimumLength(input:String, length:Int): Predicate[ErrorMessages, String] = ???

}



