/*
 * Copyright 2022 32Bytes Software LTD
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.fabianmurariu.graphs.data.dg

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of EntryIndex for ${M}")
@typeclass
trait EntryIndex[M[_, _]] extends Serializable {

  def entry[V, E](m: M[V, E])(id: Int): VertexEntry[V, E]

  def addOrUpdateEntry[V, E](m: M[V, E])(id: Int, v: V)(
    f: VertexEntry[V, E] => VertexEntry[V, E]
  ): M[V, E]

  def removeVertex[V, E](m: M[V, E])(id: Int): M[V, E]

  def vertices[V, E](m: M[V, E]): Iterable[V]

  def empty[V, E]: M[V, E]
}

object EntryIndex {

  type ImmutableEntryIndex[V, E] = Vector[VertexEntry[V, E]]

  implicit val vectorEntryIndex: EntryIndex[ImmutableEntryIndex] =
    new EntryIndex[ImmutableEntryIndex] {

      override def removeVertex[V, E](
        m: ImmutableEntryIndex[V, E]
      )(id: Int): ImmutableEntryIndex[V, E] = {
        m(id) match {
          case Empty => m
          case Entry(id, _, out, into) =>
            val g1 = out.foldLeft(m) { (g, i) =>
              g(i) match {
                case Empty => g
                case e @ Entry(_, _, _, into) =>
                  val newE = e.copy(into = into.remove(id))
                  g.updated(i, newE)
              }
            }

            val g2 = into.foldLeft(g1) { (g, i) =>
              g(i) match {
                case Empty => g
                case e @ Entry(_, _, out, _) =>
                  val newE = e.copy(out = out.remove(id))
                  g.updated(i, newE)
              }
            }
            g2.updated(id, Empty.asInstanceOf[VertexEntry[V, E]])
        }
      }

      override def addOrUpdateEntry[V, E](
        m: ImmutableEntryIndex[V, E]
      )(id: Int, v: V)(
        f: VertexEntry[V, E] => VertexEntry[V, E]
      ): ImmutableEntryIndex[V, E] = {
        if (id >= m.size) {
          // FIXME: can we get into a position where ids are greater than the size by more than 1?
          m :+ Entry(
            id,
            v,
            VecStore(Vector.empty, Vector.empty),
            VecStore(Vector.empty, Vector.empty)
          )
        } else
          m(id) match {
            case Empty =>
              m.updated(
                id,
                Entry(
                  id,
                  v,
                  VecStore(Vector.empty, Vector.empty),
                  VecStore(Vector.empty, Vector.empty)
                )
              )
            case e => m.updated(id, f(e))
          }
      }

      override def entry[V, E](m: ImmutableEntryIndex[V, E])(
        id: Int
      ): VertexEntry[V, E] = m(id)

      override def empty[V, E]: ImmutableEntryIndex[V, E] = Vector.empty

      override def vertices[V, E](m: ImmutableEntryIndex[V, E]): Iterable[V] = {
        m.collect { case Entry(_, v, _, _) =>
          v
        }
      }

    }

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[EntryIndex]] for `M`.
   */
  @inline def apply[M[_, _]](implicit instance: EntryIndex[M]): EntryIndex[M] = instance

  @deprecated("Use graph.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllEntryIndexOps[M[_, _], A, B](target: M[A, B])(implicit tc: EntryIndex[M]): AllOps[M, A, B] {
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
    def entry(id: Int): VertexEntry[A, B] = typeClassInstance.entry[A, B](self)(id)
    def addOrUpdateEntry(id: Int, v: A)(f: VertexEntry[A, B] => VertexEntry[A, B]): M[A, B] = typeClassInstance.addOrUpdateEntry[A, B](self)(id, v)(f)
    def removeVertex(id: Int): M[A, B] = typeClassInstance.removeVertex[A, B](self)(id)
    def vertices: Iterable[A] = typeClassInstance.vertices[A, B](self)
  }
  trait AllOps[M[_, _], A, B] extends Ops[M, A, B]
  trait ToEntryIndexOps extends Serializable {
    implicit def toEntryIndexOps[M[_, _], A, B](target: M[A, B])(implicit tc: EntryIndex[M]): Ops[M, A, B] {
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
