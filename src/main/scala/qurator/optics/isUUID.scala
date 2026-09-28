package qurator.optics

import java.util.UUID

import qurator.util.Derive

import monocle.Iso

trait IsUUID[A] {
  def _UUID: Iso[UUID, A]
}

object IsUUID {
  def apply[A: IsUUID]: IsUUID[A] = implicitly

  implicit val identityUUID: IsUUID[UUID] = new IsUUID[UUID] {
    val _UUID = Iso[UUID, UUID](identity)(identity)
  }

  def opaqueUUID[A <: UUID](implicit cast: UUID => A): IsUUID[A] = new IsUUID[A] {
    val _UUID = Iso[UUID, A](cast)((a: A) => a.asInstanceOf[UUID])
  }
}

object uuid extends Derive[IsUUID]
