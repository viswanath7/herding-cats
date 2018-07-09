package com.example.cats.casestudy.asynchronous

import cats.Monoid

object MonoidInstance {
	
	type T[K, V] = Traversable[Tuple2[K, V]]
	
	/**
		* Reduces a collection of pairs containing a generic-key and a numeric-value where the reduction operation is summation of numeric values
		*
		* @param collection
		* @param numeric
		* @tparam K
		* @tparam V
		* @return Collection reduced by key
		*/
	def reduceByKey[K,V](collection: T[K, V])(implicit numeric: Numeric[V]): T[K, V] = {
		
		import numeric._
		
		val reducedByKey: Map[K, V] = collection.groupBy(_._1)
			.map {
				case (group: K, traversable) => traversable.reduce {
					(a, b) => (a._1, a._2 + b._2)
				}
			}
		
		reducedByKey.toTraversable
	}
	
	implicit def traversableTuplesMonoid[K,V](implicit numeric: Numeric[V]): Monoid[T[K, V]] =
		new Monoid[T[K, V]] {
			
			/**
				* Identity element for this Monoid
				* @return
				*/
			override def empty: (T[K, V]) = Traversable()
			
			/**
				* Associative operation taking which combines two values.
				* @param x
				* @param y
				* @return
				*/
			override def combine(x: T[K, V], y: T[K,V]): T[K,V] = reduceByKey( (x.toIterator ++ y.toIterator).toTraversable )
			
		}
}