package com.example.cats.monad

class EvalMonadUsage {
	
	import cats.Eval
	
	/**
		* Recursive factorial function that's safe from stack overflow
		*
		* @param n  Number for which factorial must be computed
		* @return Factorial of the number
		*/
	def factorial(n: BigInt): Eval[BigInt] =
		if(n == 1) { Eval.now(n) }
		else { Eval.defer(factorial(n - 1).map(_ * n)) }
	
	
	
	/**
		* Non stack safe implementation of foldRight
		*
		* @param listOfItems
		* @param accumulator
		* @param fn
		* @tparam A
		* @tparam B
		* @return
		*/
	def foldRight[A, B](listOfItems: List[A], accumulator: B)(fn: (A, B) => B): B =
		listOfItems match {
		case head :: tail => fn(head, foldRightRedefined(tail, accumulator)(fn))
			// The function call is not tail recursive here so it can blow up the call-stack
		case Nil => accumulator
	}
	
	
	def foldRightEval[A, B](listOfItems: List[A], accumulator: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
		listOfItems match {
			case head :: tail => Eval.defer(fn(head, foldRightEval(tail, accumulator)(fn)))
			case Nil => accumulator
		}
	
	def foldRightRedefined[A, B](listOfItems: List[A], accumulator: B)(fn: (A, B) => B): B = {
		val bEvaluation = foldRightEval(listOfItems, Eval.now(accumulator)) {
			(a, bEval) => bEval.map(fn(a, _))
		}
		bEvaluation.value
	}
	
}

object EvalMonadUsage extends App {
	
	val evalMonad = new EvalMonadUsage
	
	println(s" Computing factorial of ${evalMonad.factorial(120).value} ")
	
}
