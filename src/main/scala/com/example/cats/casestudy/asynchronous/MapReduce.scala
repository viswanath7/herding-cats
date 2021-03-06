package com.example.cats.casestudy.asynchronous
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.syntax.traverse._
import cats.{Foldable, Monoid, Traverse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
class MapReduce {
	
	def availableCores: Int = Runtime.getRuntime.availableProcessors
	
	/**
		* Implements map-reduce on a single machine by following the steps listed below
		*  <li> Partitions the batch of input data and sends a partition to one of the available CPUs / cores on the machine
		*  <li> Performs a local map-reduce on each partition of batch
		*  <li> Combines the result using Monoid
		*
		* @param batchInput Batch of input data
		* @param transformation Transformation function
		* @tparam A Input type
		* @tparam B Output type
		* @return Combined end-result that's eventually computed
		*/
	def parallelFoldMapV1[A, B : Monoid](batchInput: Vector[A])(transformation: A => B): Future[B] = {
		// The data type Vector is used as we're using 'grouped' method
		// Vector provides fast random access
		
		val partitionSize: Int = (batchInput.size.toDouble / availableCores).ceil.toInt
		
		val mapPhaseResultForEachPartition: Iterator[Future[B]] = batchInput
			.grouped(partitionSize)
			.map {
				partition =>
					Future {
						// Type B is a monoid so one can use the combine function |+| to combines values of type B
						partition.foldLeft(Monoid[B].empty)((b, a) => b |+| transformation(a))
					}
			}
		
		// Combining results from each partition
		Future.sequence(mapPhaseResultForEachPartition)
			.map( partitionResult => partitionResult.foldLeft(Monoid[B].empty)(_|+|_) )
	}
	
	/**
		* Equivalent version that utilises foldMap from Foldable as an improvement
		*/
	def parallelFoldMapV2[A, B : Monoid](batchInput: Vector[A])(transformation: A => B): Future[B] = {
		import cats.Foldable
		
		val partitionSize: Int = (batchInput.size.toDouble / availableCores).ceil.toInt
		
		// foldMap maps a user-supplied func􏰀tion over the sequence and combines the results using a Monoid.
		val mapPhaseResultForEachPartition: Iterator[Future[B]] = batchInput
			.grouped(partitionSize)
  		.map( partition => Future( Foldable[Vector].foldMap(partition)(transformation) ))
		
		// Combining results from each partition
		Future.sequence(mapPhaseResultForEachPartition)
			.map( partitionResult => partitionResult.foldLeft(Monoid[B].empty)(_|+|_) )
			
	}
	
	/**
		* Equivalent version of fold map that utilises Cat's traversable and foldable
		*/
	def parallelFoldMap[A, B : Monoid](batchInput: Vector[A])(transformation: A => B) = {
		
		
		
		val partitionSize: Int = (batchInput.size.toDouble / availableCores).ceil.toInt
		
		// foldMap maps a user-supplied func􏰀tion over the sequence and combines the results using a Monoid.
		val eventualVector: Future[Vector[B]] = batchInput
			.grouped(partitionSize)
			.toVector
			.traverse(partition => Future(partition.foldMap(transformation)))
		
		eventualVector.map(_.combineAll)
	}
	
}

object MapReduce extends App {
	
	import cats.instances.int._

	import scala.concurrent.Await
	import scala.concurrent.duration._
	import scala.language.postfixOps
	
	val mapReduce = new MapReduce
	
	/*private val eventualResult1: Future[Int] = mapReduce.parallelFoldMapV1( (1 to 10).toVector )(identity)
	private val eventualResult2: Future[Int] = mapReduce.parallelFoldMapV2( (1 to 100).toVector )(identity)*/
	private val eventualResult3: Future[Int] = mapReduce.parallelFoldMap( (1 to 1000).toVector )(identity)
	
	/*println(s"Sum of numbers in range 1 to 10: ${Await.result(eventualResult1, 2 seconds)}")
	println(s"Sum of numbers in range 1 to 100: ${Await.result(eventualResult2, 2 seconds)}")*/
	println(s"Sum of numbers in range 1 to 1000: ${Await.result(eventualResult3, 2 seconds)}")
}




object WordCount extends App {
	
	import MonoidInstance.{T, traversableTuplesMonoid}
	import cats.instances.string._

	import scala.collection.immutable.ListMap
	import scala.concurrent.Await
	import scala.concurrent.duration._
	import scala.language.postfixOps
	
	val mapReduce = new MapReduce
	
	val linesOfText = "Betty Botter bought some butter " +
		"But she said this butter is bitter " +
		"If I put it in my batter " +
		"It will make my batter bitter " +
		"But a bit of better butter " +
		"Will surely make my batter better " +
		"So she bought a bit of butter " +
		"Better than her bitter butter " +
		"And she put it in her batter " +
		"And her batter was not bitter " +
		"So it was better Betty Botter " +
		"Bought a bit of better butter "
	
	def getWords(input:String) = input.split("\\W+").map(_.toLowerCase).toVector
	
	
	private val stringToTraversableTuples: String => T[String, Int] =
		word => List[Tuple2[String, Int]]( (word, 1) )
	
	val eventualResult = mapReduce.parallelFoldMap( getWords(linesOfText) )(stringToTraversableTuples)
	val wordCount: T[String, Int] = Await.result(eventualResult, 2 seconds)
	val sortedResult: ListMap[String, Int] = ListMap(wordCount.toSeq.sortBy(_._1):_*)
	
	println(s"Word count in text: ${ sortedResult }")
}
