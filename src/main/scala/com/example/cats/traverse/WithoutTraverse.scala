package com.example.cats.traverse

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object WithoutTraverse extends App {
	
	val hostnames = List("alpha.example.com", "beta.example.com", "gamma.example.com")
	
	def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60) // just for demonstration
	
	/**
		* Objective: Reduce the results to a single Future
		* The type of accumulator must be identical to return type
		*/
	val allUptimes: Future[List[Int]] = hostnames.foldLeft(Future(List.empty[Int])) {
		(accumulatorFuture, hostName) =>
			for {
				accumulator  <- accumulatorFuture
				uptime <- getUptime(hostName)
			} yield accumulator :+ uptime
	}
	
	println(s"Uptimes: ${Await.result(allUptimes, 1.second)}")
	
}



