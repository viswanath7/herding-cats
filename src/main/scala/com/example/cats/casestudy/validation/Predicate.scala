package com.example.cats.casestudy.validation

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply._
import cats.syntax.semigroup._ // for |+|
import cats.syntax.validated._

/**
	* A wrapper for function (A => Validated[E, A]) that lets us compose functions
	*
	* @tparam E Error type
	* @tparam A Input type
	*/
sealed trait Predicate[E, A] {
	
	import com.example.cats.casestudy.validation.Predicate.{And, Or, Pure}
	
	def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
	
	/**
		* Required by Kleisli.
		* A kleisli composes functions of type (A => F[B]) where the wrapped function returns a Monad
		*
		* @param semigroup  Semi-group for Error type
		* @return
		*/
	def run(implicit semigroup: Semigroup[E]): A => Either[E, A] =
		(input: A) => this(input).toEither
	
	def apply(input: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
		
		case Pure(func) => func(input)
		
		case And(left, right) => (left(input), right(input)).mapN((_, _) => input)
		/**
		   Internally mapN uses the Semigroupal to extract the values from the Validated
			 and the Functor to apply the values to the function.
		
			 The statement above is equivalent to one below
			 case And(left, right) => (left(a), right(a)).mapN((result1:A, result2:A) => a)
				
			 The function argument for mapN defines how successful un-boxed / values lifted from context must be combined
			 Semigroupal's "combine" method is used to combine error values so that doesn't have to be defined. It's implicit.
			 The function argument for mapN must be defined only for un-boxed values of successful scenarios
			 The successful scenario for Validated[E,A] is Valid[A] and its value lifted / un-boxed is of type A
			*/
			
		case Or(left, right) => left(input) match {
			case leftSuccess: Valid[A] => leftSuccess
			case Invalid(leftError) => right(input) match {
				case rightSuccess:Valid[A] => rightSuccess
				case Invalid(rightError) => Invalid(leftError |+| rightError)
			}
		}
		
	}
}

object Predicate {
	
	final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
	final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
	final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
	
	def apply[E, A](fn: A => Validated[E, A]): Predicate[E, A] = Pure(fn)
	
	def lift[E, A](error: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if(fn(a)) a.valid else error.invalid)
	
}


