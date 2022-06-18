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

package com.github.fabianmurariu.graphs.kernel

import simulacrum.typeclass
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of Graph for ${G}")
@typeclass
trait Graph[G[_, _]] extends Serializable {
  def neighbours[V, E](g: G[V, E])(v: V): Iterable[V]
  def isEmpty[V, E](g: G[V, E]): Boolean
}

object Graph {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /**
   * Summon an instance of [[Graph]] for `G`.
   */
  @inline def apply[G[_, _]](implicit instance: Graph[G]): Graph[G] = instance

  @deprecated("Use graph.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllGraphOps[G[_, _], A, B](target: G[A, B])(implicit tc: Graph[G]): AllOps[G, A, B] {
      type TypeClassType = Graph[G]
    } = new AllOps[G, A, B] {
      type TypeClassType = Graph[G]
      val self: G[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[G[_, _], A, B] extends Serializable {
    type TypeClassType <: Graph[G]
    def self: G[A, B]
    val typeClassInstance: TypeClassType
    def neighbours(v: A): Iterable[A] = typeClassInstance.neighbours[A, B](self)(v)
    def isEmpty: Boolean = typeClassInstance.isEmpty[A, B](self)
  }
  trait AllOps[G[_, _], A, B] extends Ops[G, A, B]
  trait ToGraphOps extends Serializable {
    implicit def toGraphOps[G[_, _], A, B](target: G[A, B])(implicit tc: Graph[G]): Ops[G, A, B] {
      type TypeClassType = Graph[G]
    } = new Ops[G, A, B] {
      type TypeClassType = Graph[G]
      val self: G[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use graph.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToGraphOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}

