package com.example.cats.semigroupal

/**
	* To ensure consistent semantics, Cats’ Monad (which extends Semigroupal)
	* provides a standard definition of product in terms of map and flatMap.
	*
	* The consistency of semantics is important for higher level abstractions
	*/
import cats.Monad

import cats.syntax.flatMap._
import cats.syntax.functor._
import scala.language.higherKinds

object SemigroupalAppliedToMonads {
	
	// definition of product in terms of map and flat-map
	def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = x.flatMap(a => y.map(b => (a, b)))
	// definition of product in terms of for comprehension
	// def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = for { a <- x; b <- y } yield (a, b)
	
}