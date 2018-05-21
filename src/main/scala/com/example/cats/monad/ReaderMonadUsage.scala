package com.example.cats.monad

import cats.data.Reader
import cats.syntax.applicative._

/**
	* Class representing configuration that shall be injected
	* as a input to the system with dependencies
	*
	* @param userIdUserNameMapping
	* @param userNamePasswordMapping
	*/
case class Configuration(userIdUserNameMapping: Map[Int, String], userNamePasswordMapping: Map[String, String])


class ReaderMonadUsage {
	
	// Configuration object shall be injected as an dependency to build the system
	
	/**
		* Type alias for configuration reader
		* that consumes Configuration as an input
		* to inject it as a dependency
		* @tparam T
		*/
	type ConfigurationReader[T] = Reader[Configuration, T]
	
	/**
		* @param userId identifier of an user
		* @return sought username for supplied user identifier
		*/
	def findUsername(userId: Int): ConfigurationReader[Option[String]] =
		Reader(conf => conf.userIdUserNameMapping.get(userId))
	
	/**
		* Verifies if the supplied user name corresponds to supplied password
		*
		* @param userName
		* @param password
		* @return
		*/
	def checkPassword(userName: String, password: String): ConfigurationReader[Boolean] =
		Reader( conf => conf.userNamePasswordMapping.get(userName).exists(_.contentEquals(password)) )
	
	
	def checkLogin(userId: Int, password: String): ConfigurationReader[Boolean] = for {
			loginName <- findUsername(userId)
			isMatch <- loginName
				.map(checkPassword(_, password))
				.getOrElse(false.pure[ConfigurationReader])
	} yield isMatch
	
}

object ReaderMonadUsage extends App {
	
	val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")
	val passwords = Map("dade"  -> "zerocool", "kate"  -> "acidburn", "margo" -> "secret")
	
	val conf = Configuration(users, passwords)
	
	val demo = new ReaderMonadUsage
	
	val result1 = demo.checkLogin(1, "zerocool").run(conf)
	val result2 = demo.checkLogin(2, "wrong-password").run(conf)
	
	println(s"Does user-id 1 match password 'zerocool'? $result1")
	println(s"Does user-id 2 match password 'wrong-password'? $result2")
}


