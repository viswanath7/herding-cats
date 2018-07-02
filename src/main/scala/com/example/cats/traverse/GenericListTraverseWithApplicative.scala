package com.example.cats.traverse
import cats.Applicative
import cats.data.Validated
import cats.instances.option._
import cats.instances.vector._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}

object GenericListTraverseWithApplicative extends App {
	
	
	def listTraverse[F[_] : Applicative, A, B](input: List[A])(transformationFn: A => F[B]) =
		input.foldLeft(List.empty[B].pure[F]) {
			(accumulator, item) => (accumulator, transformationFn(item)).mapN(_ :+ _)
		}
	
	def listTraverseWithIdentity[F[_] : Applicative, T](list: List[F[T]]): F[List[T]] = listTraverse(list)(identity)
	
	val maybeInts: Option[List[Int]] = listTraverseWithIdentity[Option, Int](List(Some(1), Some(2), Some(3)))
	
	println(s"Converting List[F[T]] to F[List[T]]")
	println(s"Result of converting List[Option[Int]] to Option[List[Int]] =  $maybeInts")
	
	println(s"List vector traverse - Example 1:  ${listTraverseWithIdentity(List(Vector(1, 2), Vector(3, 4)))}")
	println(s"List vector traverse - Example 2:  ${listTraverseWithIdentity(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))}")
	
	def onlyEvens(numbers: List[Int]) = listTraverse[Option, Int, Int](numbers) { num => if (num % 2 == 0) Some(num) else None }
	
	println(s"Only evens: ${onlyEvens(List(2, 4, 6))}")
	println(s"Only evens: ${onlyEvens(List(1,2,3))}")
	
	type ErrorsOr[A] = Validated[List[String], A]
	
	def onlyOdds(input:List[Int]) = listTraverse[ErrorsOr,Int,Int](input) {
		num => if (num%2 != 0) Validated.valid(num) else Validated.invalid(List(s"$num is even!"))
	}
	
	println(s"Only odds: ${onlyOdds( List(1,2,3,4) )}")
	
}
