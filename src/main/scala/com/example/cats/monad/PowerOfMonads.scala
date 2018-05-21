package com.example.cats.monad

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.language.higherKinds


object PowerOfMonads {
	
	def square[A, T >: A](value:A)(implicit num: Numeric[T]): T = num.times(value, value)
	def sum[A, T >: A](first:A, second:A)(implicit num: Numeric[T]): T = num.plus(first, second)
	
	
	
	/*def sumOfSquares[T](first:T, second:T)(implicit num: Numeric[T]): Id[Int] = {
		println("Invoked version 1")
		import cats.syntax.applicative._
		val identityMonad = implicitly[Monad[Id]]
		sumOfSquares(first.pure[Id], second.pure[Id])
	}*/
	
	/**
		* Ultimate sum of squares method
		*
		* @param x First value in context
		* @param y Second value in context
		* @tparam F Monadic context
		* @tparam T Type parameter in the Monad
		* @return Sum of squares of first and second values in the Monadic context
		*/
	def sumOfSquares[F[_]: Monad, A, T >: A](x: F[A], y: F[A])(implicit num: Numeric[T]) : F[T] = {
		def square(value:T): T = num.times(value, value)
		def sum(first:T, second:T): T = num.plus(first, second)
		for {
			first <- x
			second <- y
		} yield sum(square(first), square(second))
	}
	
}
