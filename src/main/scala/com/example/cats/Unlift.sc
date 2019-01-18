import scala.language.higherKinds

import cats.{Functor, Id}
import cats.data.Const


object CatsConst extends App {
	
	trait Lens[S, A] {
		
		def modifyF[F[_] : Functor](s: S)(f: A => F[A]): F[S]
		
		def set(s: S, a: A): S = modify(s)(_ => a)
		
		def modify(s: S)(f: A => A): S = modifyF[Id](s)(f)
		
		def get(s: S): A = {
			val storedValue = modifyF[Const[A, ?]](s)(a => Const(a))
			storedValue.getConst
		}
	}
	
	class LensImpl extends Lens[String, Int] {
		
		private val cache: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		
		override def modifyF[F[_] : Functor](key: String)(f: Int => F[Int]): F[String] = {
			val x: Option[F[Int]] = cache.get(key).map(f(_))
			x match {
				case Some(v) => cache.put(key, v)
				case None    => ???
			}
			???
		}
	}
	
	val lens = new LensImpl
	lens.set("1", 1)
	println(lens.get("1"))
