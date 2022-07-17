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
import com.github.fabianmurariu.graphs.ir.{Query, Ref}

@implicitNotFound("Could not find an instance of Graph for ${G}")
@typeclass
trait Graph[G[_, _]] extends Serializable with EvalGraph[G]{
  def out[V, E](g: G[V, E])(vs: Rs[V]): Rs[V]
  def in[V, E](g: G[V, E])(vs: Rs[V]): Rs[V]

  def isEmpty[V, E](g: G[V, E]): Boolean
  def vertices[V, E](g: G[V, E]): Rs[V]

  def addVertex[V, E](g: G[V, E])(v: V): G[V, E]
  def addEdge[V, E](g: G[V, E])(src: V, dst: V, e: E): G[V, E]
  def removeVertex[V, E](g: G[V, E])(v: V): G[V, E]
  def removeEdge[V, E](g: G[V, E])(src: V, dst: V, e: E): G[V, E]

  def get[V, E](g: G[V, E])(v: V): Option[V]

  // this should be implementable here
  def eval[V, E, O <: Ref](g: G[V,E])(q: Query[V,E,O]): Rs[O] = ???
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
    def out(vs: Rs[A]): Rs[A] = typeClassInstance.out[A, B](self)(vs)
    def in(vs: Rs[A]): Rs[A] = typeClassInstance.in[A, B](self)(vs)
    def isEmpty: Boolean = typeClassInstance.isEmpty[A, B](self)
    def vertices: Rs[A] = typeClassInstance.vertices[A, B](self)
    def addVertex(v: A): G[A, B] = typeClassInstance.addVertex[A, B](self)(v)
    def addEdge(src: A, dst: A, e: B): G[A, B] = typeClassInstance.addEdge[A, B](self)(src, dst, e)
    def removeVertex(v: A): G[A, B] = typeClassInstance.removeVertex[A, B](self)(v)
    def removeEdge(src: A, dst: A, e: B): G[A, B] = typeClassInstance.removeEdge[A, B](self)(src, dst, e)
    def get(v: A): Option[A] = typeClassInstance.get[A, B](self)(v)
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
