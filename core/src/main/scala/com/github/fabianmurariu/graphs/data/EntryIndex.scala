package com.github.fabianmurariu.graphs.data

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of EntryIndex for ${M}")
@typeclass
trait EntryIndex[M[_, _]] extends Serializable {

  def entry[V, E](m: M[V, E])(id: Int): VertexEntry[V, E]

  def empty[V, E]: M[V, E]
}

object EntryIndex {

  type ImmutableEntryIndex[V, E] = Vector[VertexEntry[V, E]]

  implicit val vectorEntryIndex: EntryIndex[ImmutableEntryIndex] =
    new EntryIndex[ImmutableEntryIndex] {

      override def entry[V, E](m: ImmutableEntryIndex[V, E])(
          id: Int
      ): VertexEntry[V, E] = m(id)

      override def empty[V, E]: ImmutableEntryIndex[V, E] = Vector.empty

    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /** Summon an instance of [[EntryIndex]] for `M`.
    */
  @inline def apply[M[_, _]](implicit instance: EntryIndex[M]): EntryIndex[M] =
    instance

  @deprecated("Use graph.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllEntryIndexOps[M[_, _], A, B](
        target: M[A, B]
    )(implicit tc: EntryIndex[M]): AllOps[M, A, B] {
      type TypeClassType = EntryIndex[M]
    } = new AllOps[M, A, B] {
      type TypeClassType = EntryIndex[M]
      val self: M[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[M[_, _], A, B] extends Serializable {
    type TypeClassType <: EntryIndex[M]
    def self: M[A, B]
    val typeClassInstance: TypeClassType
    def entry(id: Int): VertexEntry[A, B] =
      typeClassInstance.entry[A, B](self)(id)
  }
  trait AllOps[M[_, _], A, B] extends Ops[M, A, B]
  trait ToEntryIndexOps extends Serializable {
    implicit def toEntryIndexOps[M[_, _], A, B](
        target: M[A, B]
    )(implicit tc: EntryIndex[M]): Ops[M, A, B] {
      type TypeClassType = EntryIndex[M]
    } = new Ops[M, A, B] {
      type TypeClassType = EntryIndex[M]
      val self: M[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use graph.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToEntryIndexOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
