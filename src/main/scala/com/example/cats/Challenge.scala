package com.example.cats

import scala.language.higherKinds

import scala.language.implicitConversions
import cats.arrow.FunctionK
import cats.{Functor, Id}
import cats.~>


object Challenge {
	
	import scala.concurrent.Future
	
	
	implicit def unlift[F[_] : Functor, A](input:F[A])(implicit fn: F ~> Id): A = fn(input)
	
	/*
	
	implicit def futureExtractor[Future, A](input:Future[A]): Future ~> Id = {
		λ[Future ~> Id]{
			import scala.concurrent.ExecutionContext.Implicits.global
			import scala.concurrent.Await
			import scala.concurrent.duration.Duration
			Await.result(input, Duration.Inf)
		}
	}
	
	implicit def futureExtractor[List, A](input:List): List ~> Id = {
		λ[List ~> Id]{
			import scala.concurrent.ExecutionContext.Implicits.global
			import scala.concurrent.Await
			import scala.concurrent.duration.Duration
			Await.result(_, Duration.Inf)
		}
	}
	*/
	implicit def listExtractor: FunctionK[List, Id] = new FunctionK[List, Id] {
		def apply[A](list: List[A]): Id[A] = list.head
	}
	
	implicit def futureExtractor: FunctionK[Future, Id] = new FunctionK[Future, Id] {
		def apply[A](future: Future[A]): Id[A] = {
			import scala.concurrent.ExecutionContext.Implicits.global
			import scala.concurrent.Await
			import scala.concurrent.duration.Duration
			Await.result(future, Duration.Inf)
		}
	}
	
	
	
}
