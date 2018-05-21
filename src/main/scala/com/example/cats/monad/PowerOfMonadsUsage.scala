package com.example.cats.monad

import cats.Id
import cats.instances.future._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object PowerOfMonadsUsage extends App {
	
	// Using generic squares for numeric types
	println(s"Square of integer ${PowerOfMonads.square(5)}")
	println(s"Square of double ${PowerOfMonads.square(5.0)}")
	// Using generic summation for numeric types
	println(s"Sum of long ${PowerOfMonads.sum(2l, 3l)}")
	println(s"Sum of float ${PowerOfMonads.sum(2f, 3f)}")
	
	// Using monadic method that uses composition
	println(s"Sum of squares for list ${PowerOfMonads.sumOfSquares(  List(1,2,3), List(1,2,3) )}")
	println(s"Sum of squares for options ${PowerOfMonads.sumOfSquares(  Option(1), 2.pure[Option] )}")
	println(s"Sum of squares for future ${PowerOfMonads.sumOfSquares(  1.pure[Future], Future(2) ).value}")
	println(s"Sum of squares for id ${PowerOfMonads.sumOfSquares(1.pure[Id], 2.pure[Id])}")
	// Using implicit conversion for type class
	
	import cats.syntax.applicative._

	import scala.language.implicitConversions
	
	/**
		* Implicit conversion for any numeric type T to Id[T]
		* @param value value with type T
		* @tparam T numeric type
		* @return numeric type wrapped in the context of an Identity Monad
		*/
	implicit def anyNum2Id[T](value:T)(implicit num: Numeric[T]):Id[T] = {
		println(s"Converting value $value to an identity type")
		value.pure[Id]
	}
	// println(s"Sum of squares for int ${PowerOfMonads.sumOfSquares(1, 2)}")
}
