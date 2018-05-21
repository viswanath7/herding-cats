package com.example.cats.monad

import cats.MonadError
import cats.instances.either._
import cats.syntax.either._

object MonadErrorUsage extends App {
	
	private val demo = new MonadErrorUsage
	
	def computeFactorial(numbers: Int*): Unit = {
		numbers.foreach{ number =>
			val result = demo.factorial(number)
			result.toOption match {
				case Some(factorial) => println(s"Factorial of $number is $factorial")
				case None => println(s"Error encountered during computation of $number factorial : ${result.left.get}")
			}
		}
	}
	
	computeFactorial(10, 110, -10)
}

class MonadErrorUsage {
	
	// Type alias for Either to capture error message or result
	private type ErrorOr[A] = Either[String, A]
	
	private[this] val monadError = MonadError[ErrorOr, String]
	
	/**
		* Function that computes factorial for a natural number i.e. a non negative integer
		*
		* @param accumulator
		* @param input
		* @return
		*/
	private[this] def factorial(accumulator: Int, input:ErrorOr[Int]) :ErrorOr[Int] = {
		
		val validatedInput = monadError.ensure(input)("Positive number less than 100 must be supplied as input!")(num => num > 0 && num <= 100)
		
		validatedInput match {
			case Right(num) if num > 1 => factorial(num * accumulator, (num -1).asRight);
			case Right(num) => accumulator.asRight
			case Left(x) => validatedInput
		}
	}
	
	def factorial(num: Int): ErrorOr[Int] = factorial(1, num.asRight)
	
}