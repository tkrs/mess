package mess.internal

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

private[mess] object ScalaVersionSpecifics {
  private[mess] type Factory[-E, +T] = CanBuildFrom[Nothing, E, T]

  private[mess] implicit class FactoryOps[F, E, T](val bf: Factory[E, T]) extends AnyVal {
    def newBuilder: mutable.Builder[E, T] = bf.apply()
  }
}
