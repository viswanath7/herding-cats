package com.example.cats.traverse

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.language.postfixOps
import cats.Applicative
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global


object TraverseWithApplicative extends App {
	
	val hostnames = List("alpha.example.com", "beta.example.com", "gamma.example.com")
	
	def getUptime(hostname: String): Future[Int] = Future(hostname.length * 60) // just for demonstration
	
	val eventualInts: Future[List[Int]] = List.empty[Int].pure[Future]
	
	val allUptimes: Future[List[Int]] = hostnames.foldLeft(eventualInts) {
		(accumulator, hostName) => ( accumulator, getUptime(hostName) )
			.mapN( (acc,ut) => acc :+ ut )
	}
	
	println(s"Uptimes: ${Await.result(allUptimes, 1 second)} ")
	
	
}
