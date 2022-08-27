package com.github.fabianmurariu.graphs.kernel

import simulacrum.typeclass
import scala.annotation.implicitNotFound
import scala.collection.immutable.Queue

@implicitNotFound("Could not find an instance of TraverseSupport for ${F}")
@typeclass
trait TraverseSupport[F[_]] extends Serializable {

  def push[A](f: F[A])(a: A): F[A]

  def pop[A](f: F[A]): Option[(A, F[A])]

  def fromIter[A](iter: Iterable[A]): F[A]
}

object TraverseSupport {

  implicit val queueInstance = new TraverseSupport[Queue] {

    override def push[A](f: Queue[A])(a: A): Queue[A] = f.enqueue(a)

    override def pop[A](f: Queue[A]): Option[(A, Queue[A])] = f.dequeueOption

    override def fromIter[A](iter: Iterable[A]): Queue[A] = Queue.from(iter)

  }

  implicit val listInstance = new TraverseSupport[List] {

    override def push[A](f: List[A])(a: A): List[A] = a :: f

    override def pop[A](f: List[A]): Option[(A, List[A])] =
      f match {
        case head :: next => Some((head, next))
        case Nil          => None
      }

    override def fromIter[A](iter: Iterable[A]): List[A] = List.from(iter)

  }
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[TraverseSupport]] for `F`.
   */
  @inline def apply[F[_]](implicit instance: TraverseSupport[F]): TraverseSupport[F] = instance

  @deprecated("Use graph.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllTraverseSupportOps[F[_], A](target: F[A])(implicit tc: TraverseSupport[F]): AllOps[F, A] {
      type TypeClassType = TraverseSupport[F]
    } = new AllOps[F, A] {
      type TypeClassType = TraverseSupport[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[F[_], A] extends Serializable {
    type TypeClassType <: TraverseSupport[F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def push(a: A): F[A] = typeClassInstance.push[A](self)(a)
    def pop: Option[(A, F[A])] = typeClassInstance.pop[A](self)
  }
  trait AllOps[F[_], A] extends Ops[F, A]
  trait ToTraverseSupportOps extends Serializable {
    implicit def toTraverseSupportOps[F[_], A](target: F[A])(implicit tc: TraverseSupport[F]): Ops[F, A] {
      type TypeClassType = TraverseSupport[F]
    } = new Ops[F, A] {
      type TypeClassType = TraverseSupport[F]
      val self: F[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use graph.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToTraverseSupportOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */




}
