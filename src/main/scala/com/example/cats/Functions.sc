import cats.{Functor, Semigroupal}

import scala.language.higherKinds

def foo[A,B,C]: A => B => C = ???

trait Apply[F[_]] extends Semigroupal[F] with Functor[F] {
	
	def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
	
	def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
		ap(map(fa)(a => (b: B) => (a, b)))(fb)
}


List(1, 2, 3).foldRight(0)(_ + _)
List(1,2,3).foldRight(0)(_ - _) //(1-(2-(3-0)))

import io.circe.syntax._
Map("foo" -> "bar", "key" -> "value").asJson.noSpaces

import cats._
import cats.data._
import cats.implicits._
Map("foo" -> "bar", "key" -> "value").asJson.noSpaces.some


