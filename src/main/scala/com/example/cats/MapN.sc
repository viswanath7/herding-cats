final case class Cat(name: String, born: Int, color: String)

import cats.Semigroupal
import cats.data.Validated
import cats.instances.option._
import cats.syntax.apply._     // for tupled and mapN

/**
	* Semigroup allows us to join values
	* Semigroupal allows us to join contexts
	*/
import cats.instances.list._
val listProduct = Semigroupal[List].product(List(1), List(2))

type ErrorOr[T] = Either[String, T]
import cats.instances.either._
val eitherProduct = Semigroupal[ErrorOr].product(Left("abc"), Right(123))

println(listProduct)

(Option(123), Option("abc"), Option(true)).tupled

/**
	* Internally mapN uses the Semigroupal to extract the values from the Option
	* and the Functor to apply the values to the function.
	*/
val someCat = ( Option("Garfield"), Option(1978), Option("Orange & black") ).mapN(Cat.apply)

println(someCat)


