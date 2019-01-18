package com.example.cats.casestudy.validation

import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.instances.either._
import cats.instances.list._

object Validations {
	
	/**
		* Type for storing error messages
		*/
	type Errors = NonEmptyList[String]
	
	/**
		* Type for storing result that's not erroneous
		*
		* @tparam A Type of non-erroneous value
		*/
	type Result[A] = Either[Errors, A]
	
	/**
		* Type holding Kleisli functions of type (A => F[B]) where, F[_] is fixed as Either[NonEmptyList[String], _]
		*
		* @tparam A Type of input
		* @tparam B Type of non-erroneous output
		*/
	type Verify[A, B] = Kleisli[Result, A, B]
	
	
	def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)
	
	def verify[A, B](func: A => Result[B]): Verify[A, B] = Kleisli(func)
	
	def checkPredicate[A](predicate: Predicate[Errors, A]): Verify[A, A] =
		Kleisli[Result, A, A](predicate.run)
	
}

object Verifications {
	
	import com.example.cats.casestudy.validation.Validations.{Verify, _}
	
	def longerThan(desiredLength: Int): Predicate[Errors, String] =
		Predicate.lift(error(s"Must be longer than $desiredLength characters"), str => str.length > desiredLength)
	
	val alphanumeric: Predicate[Errors, String] =
		Predicate.lift(error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))
	
	def contains(char: Char): Predicate[Errors, String] =
		Predicate.lift( error(s"Must contain the character $char"), str => str.contains(char))
	
	private val combinedPredicate: Predicate[Errors, String] = longerThan(3) and alphanumeric
	val checkUsername: Kleisli[Result, String, String] = checkPredicate(combinedPredicate)
	
	final case class User(username: String, email: String)
	
	//def createUser(username: String, email: String): Either[Errors, User] = (checkUsername.run(username),checkUsername.run(email)).mapN(User)
}
