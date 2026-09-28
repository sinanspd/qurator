package qurator.util

import scala.annotation.implicitNotFound


trait Derive[F[_]]{
  def instance(implicit ev: OnlyNewtypes): Nothing = ev.absurd

  @implicitNotFound("Only newtypes instances can be derived")
  abstract final class OnlyNewtypes {
    def absurd: Nothing = ???
  }
}

