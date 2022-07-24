package com.github.fabianmurariu.graphs.data

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of LookupTable for ${M}")
@typeclass
trait LookupTable[M[_]] extends Serializable {

  def lookup[A](m: M[A])(a: A): Int

  def update[A](m: M[A])(a: A): (Int, M[A])

  def empty[A]: M[A]
}

object LookupTable {
  type ImmutableLookupTable[V] = Map[V, Int]

  implicit val mapLookupTable: LookupTable[ImmutableLookupTable] =
    new LookupTable[ImmutableLookupTable] {

      override def update[A](m: ImmutableLookupTable[A])(a: A): (Int, ImmutableLookupTable[A]) = {
        m.get(a) match {
          case Some(value) => (value, m)
          case None => 
            val newId = m.values.maxOption.getOrElse(-1) +1 //FIXME: terrible inneficient
            (newId, m + (a -> newId))
        }
      }


      override def lookup[A](m: ImmutableLookupTable[A])(a: A): Int = m(a)

      override def empty[A]: ImmutableLookupTable[A] = Map.empty

    }
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[LookupTable]] for `M`.
   */
  @inline def apply[M[_]](implicit instance: LookupTable[M]): LookupTable[M] = instance

  @deprecated("Use graph.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllLookupTableOps[M[_], A](target: M[A])(implicit tc: LookupTable[M]): AllOps[M, A] {
      type TypeClassType = LookupTable[M]
    } = new AllOps[M, A] {
      type TypeClassType = LookupTable[M]
      val self: M[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[M[_], A] extends Serializable {
    type TypeClassType <: LookupTable[M]
    def self: M[A]
    val typeClassInstance: TypeClassType
    def lookup(a: A): Int = typeClassInstance.lookup[A](self)(a)
    def update(a: A): (Int, M[A]) = typeClassInstance.update[A](self)(a)
  }
  trait AllOps[M[_], A] extends Ops[M, A]
  trait ToLookupTableOps extends Serializable {
    implicit def toLookupTableOps[M[_], A](target: M[A])(implicit tc: LookupTable[M]): Ops[M, A] {
      type TypeClassType = LookupTable[M]
    } = new Ops[M, A] {
      type TypeClassType = LookupTable[M]
      val self: M[A] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use graph.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToLookupTableOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */




}
