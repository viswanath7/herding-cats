package com.example.cats.semigroupal
import cats.Monoid
import cats.Semigroupal
import cats.instances.invariant._
import cats.syntax.semigroup._
import cats.instances.boolean._
import cats.instances.int._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.apply._

/**
	*
	* Objective: Create an instance of Monoid[Cat] (where Cat is a custom class)
	* using implicit invariant and Semigroupal
	*
	* Semigroupal is a type class that allows one to combine contexts.
	*
	* It defines a 'product' function where the order in which input parameters to product function are computed is irrelevant.
	*
	*  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
	*
	*  The companion object for 'Semigroupal' defines a set of methods in addition to 'product'
	*  to join/combine multiple contexts
	*/
object SemigroupalUsage extends App {
	
	case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])
	
	// Transformation from Tuple3 to Cat
	val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _
	// Transformation from Cat to Tuple3
	val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)
	
	/**
	  * Generate an instance of Monoid[Cat] using Invariant's 'imapN' method
	  * that requires a pair of bidirectional transformation functions without the context i.e. Monoid
		*
		* Implicit parameters Invariant[Monoid] and Semigroupal[Monoid] are supplied through imports
		* Semigroupal is used to extract values from the context i.e. Monoid
		* Invariant is used to apply the extracted values to the bidirectional function pairs
	  */
	implicit val catMonoid: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[List[String]]).imapN(tupleToCat)(catToTuple)
	
	val garfield = Cat("Garfield", 1978, List("Lasagne"))
	val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))
	
	// Combine two instances of Monoid[Cat] using infix syntax
	// implicit conversion from Cat to Monoid[Cat] kicks-in
	val combinedCat: Cat = garfield |+| heathcliff
	
	println(s"Combined cat: $combinedCat")
}

