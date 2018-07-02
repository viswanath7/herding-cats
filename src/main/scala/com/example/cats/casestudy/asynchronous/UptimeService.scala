package com.example.cats.casestudy.asynchronous

import cats.{Applicative, Id}

import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.Applicative
import cats.syntax.functor._

import scala.concurrent.Future
import scala.language.higherKinds

sealed trait UpTimeClient[F[_]] {
	
	/**
		* Given the hostname of the server, polls it to return its up time in minutes
		*
		* @param hostname Name of the host for which the up time must be gathered
		* @return up time in minutes
		*/
	def getUptime(hostname: String): F[Int]
}

class TestUpTimeClient(remoteHosts: Map[String, Int]) extends UpTimeClient[Id] {
	def getUptime(hostname: String) = remoteHosts.getOrElse(hostname, 0)
}

class RealUpTimeClient(remoteHosts: Map[String, Int]) extends UpTimeClient[Future] {
	def getUptime(hostname: String):Future[Int] = Future.successful(remoteHosts.getOrElse(hostname, 0))
}

class UptimeService[F[_]:Applicative](upTimeClient: UpTimeClient[F]) {
	/**
		* Given a list of remote host names, computes their average uptime
		* @param hostNames  list of remote host names
		* @return average up time for the supplied list of remote hosts
		*/
	def getAverageUptime(hostNames: List[String]): F[Int] = {
		import cats.Traverse
		
		/**
			* Abbreviated defini􏰀on of Cat's traverse
			*
			* trait Traverse[F[_]] {
			*   def traverse[G[_]: Applicative, A, B] (inputs: F[A]) (func: A => G[B]): G[F[B]]
			* }
			*
			*/
		// val eventualUpTimes: Future[List[Int]] = Traverse[List].traverse(hostNames)(upTimeClient.getUptime)
		
		val eventualUpTimes: F[List[Int]] = hostNames.traverse(upTimeClient.getUptime)
		eventualUpTimes.map(listOfUpTimes => listOfUpTimes.sum/listOfUpTimes.size)
	}
}


object UptimeServiceApp extends App {
	
	import scala.concurrent.Await
	import scala.concurrent.duration._
	import scala.language.postfixOps
	
	val remoteHosts = Map("alpha.example.com" -> 10, "beta.example.com" -> 6, "gamma.example.com" -> 7, "delta.example.com" -> 8)
	
	val client = new TestUpTimeClient(remoteHosts)
	val service = new UptimeService(client)
	// "epsilon", "zeta", "theta", "kappa", "lambda", "sigma", "omega"
	val result = service.getAverageUptime( List("alpha.example.com", "beta.example.com", "gamma.example.com", "delta.example.com") )
	
	println(s"Average up-times of remote hosts: $result")
	
	val futureClient = new RealUpTimeClient(remoteHosts)
	val futureService = new UptimeService(futureClient)
	val futureResult = futureService.getAverageUptime( List("alpha.example.com", "beta.example.com", "gamma.example.com", "delta.example.com") )
	
	
	println(s"Average up-times of remote hosts using future variant: ${ Await.result(futureResult, 1 second) }")
	
}

