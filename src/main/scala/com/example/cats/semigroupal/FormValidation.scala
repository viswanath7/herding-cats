package com.example.cats.semigroupal

import cats.data.Validated

case class User (name:String, age:Int)

/**
	* Objective: Parse incoming request data in map and create a User object
	* and when parsing ensure the following rules
	* <ul>
	* <li>Name and age are mandatory</li>
	* <li>Name must not be blank</li>
	* <li>Age must be a non negative integer</li>
	* <ul>
	*/
object FormValidation {
	
	type FormData = Map[String, String]
	// Implements fail fast semantics for errors
	type FailFast[A] = Either[List[String], A]
	// Implements error accumulating semantics
	type FailSlow[A] = Validated[List[String], A]
	
	def readUser(implicit formData: FormData): FailSlow[User] = {
		import cats.instances.list._
		import cats.syntax.apply._
		import cats.syntax.either._   // for mapN
		
		(readName.toValidated, readAge.toValidated).mapN(User.apply _)
	}
	
	// FailSlow[User]
	
	/**
		* Extracts the 'name' field from the input map and, checks it against the validation rules for 'name' and returns Either[List[String], String]
		* @param inputData
		* @return
		*/
	private[this] def readName(implicit inputData: FormData): FailFast[String] = getValue("name").flatMap(nonBlank)
	
	/**
		* Extracts the 'age' field from the input map and, checks it against the validation rules for 'age' and returns Either[List[String], Int]
		* @param inputData
		* @return
		*/
	private[this] def readAge(implicit inputData: FormData):FailFast[Int] = getValue("age").flatMap(parseInt).flatMap(nonNegative)
	
	private[this] def getValue(fieldName:String)(implicit inputData:FormData) =
		inputData.get(fieldName)
		.toRight(List(s"Input data map is missing an entry with key '$fieldName'!"))
	
	private[this] def parseInt(number:String): Either[List[String], Int] = {
		import cats.syntax.either._
		Either.catchOnly[NumberFormatException]{number.toInt}.leftMap(numFormatEx => List("Supplied value cannot be converted to an integer"))
	}
	
	private[this] def nonBlank(input:String): Either[List[String], String] = {
		import cats.syntax.either._
		Right(input).ensure(List("Supplied value cannot be blank!"))(_.nonEmpty)
	}
	
	private[this] def nonNegative(input:Int): Either[List[String], Int] = {
		import cats.syntax.either._
		Right(input).ensure(List("Supplied value must be non negative"))(_ >= 0)
	}
}
