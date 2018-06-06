import cats.Functor

import scala.language.{higherKinds, reflectiveCalls}


/**
	* Higher-kinded types are type constructors that take
	* other types (or even other type constructor) as parameter.
	*
	* When we declare a type variable that is a type constructor,
	* we write F[_] to indicate that F needs to be provided with a type
	* to construct a concrete type.
	*/


/**
	* Type alias is used to declare a type that (for convenience sake) declares
	* * a type for unwieldy expression on the right side.
	*
	* @tparam A Input parameter of a type alias which is type represented as A
	*/
type T[A] = Option[Map[Int, A]] // Type alias that takes a parameter


/**
	* Type lambda declares an anonymous type, [for example within Functor's type i.e. '()' ]
	* Inside anonymous type declaration, we define the desired type alias (i.e. within in curly brackets {} )
	* and then access its type member with the # syntax.
	*/

/**
	*
	* @param functor
	* @tparam A
	* @tparam B
	*/

def foo[A[_,_],B](functor: Functor[({type N[C] = A[B,C]})#N])

