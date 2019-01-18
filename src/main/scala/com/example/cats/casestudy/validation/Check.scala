package com.example.cats.casestudy.validation

import cats.Semigroup
import cats.data.Validated

/**
	* Structure that allows transformation of its input of type A to output of type B
	* If an error occurs during transformation, it is represented by E
	*
	* Check is also an abstraction that allows us to compose functions of type A => F[B]
	*/
sealed trait Check[E, A, B] {
	
	import com.example.cats.casestudy.validation.Check._
	
	def apply(input: A)(implicit semigroup: Semigroup[E]): Validated[E, B]
	
	def map[C](transformationFunction: B => C): Check[E, A, C] = Map[E,A,B,C](this, transformationFunction)
	
	def flatMap[C](transformationFunction: B => Check[E, A, C]): FlatMap[E, A, B, C] = FlatMap[E, A, B, C](this, transformationFunction)
	
	def andThen[C](check: Check[E,B,C]): Check[E, A, C] = AndThen[E, A, B, C](this, check)

}

object Check {
	
	def apply[E, A](predicate: Predicate[E, A]): Check[E, A, A] = Pure(predicate)
	
	
	final case class Pure[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
		
		def apply(input: A)(implicit s: Semigroup[E]): Validated[E, A] = predicate(input)
	}
	
	final case class Map[E, A, B, C](check: Check[E, A, B], transformationFunction: B => C) extends Check[E, A, C] {
		
		def apply(input: A)(implicit s: Semigroup[E]): Validated[E, C] =
			check(input).map(transformationFunction) 	// applies a function to a valid 'Validated' value and returns a new value
	}
	
	/***
		*
		* Input (A => F[B])  flatMap( B => (A => F[C]) ) Output (A => F[C])
		*
		* @param check
		* @param transformationFunction
		* @tparam E
		* @tparam A
		* @tparam B
		* @tparam C
		*/
	final case class FlatMap[E, A, B, C](check: Check[E, A, B], transformationFunction: B => Check[E, A, C]) extends Check[E, A, C] {
		
		def apply(input: A)(implicit s: Semigroup[E]): Validated[E, C] = {
			check(input).withEither {
				either: Either[E, B] => either.flatMap (b => transformationFunction(b)(input).toEither)
			}
		}
		
	}
	
	final case class AndThen[E, A, B, C](firstCheck: Check[E, A, B],
	                                     secondCheck: Check[E, B, C]) extends Check[E, A, C] {
		def apply(input: A)(implicit s: Semigroup[E]): Validated[E, C] =
			firstCheck(input).withEither{
				either: Either[E, B] => either.flatMap(b => secondCheck(b).toEither)
			}
	}
	
	
}
