package com.example.cats.monad

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

/**
	* Objective is to capture log messages in a Writer
	* so that logs for concurrent computations are reliably separated
	*/
class WriterMonadUsage {
	
	/**
		* The writer carries two values
		* <ul>
		*   <li> A log of type Vector[String] as append operations are efficient on a vector
 		*   <li> A result of computation of type R
		* </ul>
		* @tparam R Result type
		*/
	type LoggedResult[R] = Writer[Vector[String], R]
	
	
	/**
		* Helper function that introduces artificial delay of 1 second
		*
		* @param body Single parameter function that does not take any parameters and returns a result of type A
		* @tparam R Result type
		* @return
		*/
	def injectArtificialDelay[R](body: => R): R = try body finally Thread.sleep(1)
	
	
	def factorial(num: Long): LoggedResult[Long] =
		for {
			result <- injectArtificialDelay {
									if(num == 0l || num == 1l) 1l.pure[LoggedResult] // Create a writer containing an empty log but a result
									else factorial(num - 1).map(_*num)
								}
			_ <- Vector(s"Factorial of $num is $result").tell // Create a writer containing a log but no result
		} yield result // map and flatMap methods append the logs from the Writer preserving it.
}

object WriterMonadUsage extends App {
	
	import scala.concurrent.ExecutionContext.Implicits.global
	
	private val demo = new WriterMonadUsage
	
	private val resultOfParallelComputation: Vector[demo.LoggedResult[Long]] =
		Await.result(
			Future.sequence(
				Vector(
					Future(demo.factorial(10)),
					Future(demo.factorial(15))
				)
			), 6 seconds
		)
	
	resultOfParallelComputation.foreach {
		_.bimap(
			computationLogs => computationLogs.foreach(println),
			factorial => println(s"Result = $factorial")
		)
	}
	
	//println(s"Factorial of 999 is ${demo.factorial(70)}")
	
}
