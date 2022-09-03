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
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.reflect.ClassTag

@implicitNotFound("Could not find an instance of Graph for ${G}")
@typeclass
trait Graph[G[_, _]] extends Serializable { self =>
  def out[V, E](g: G[V, E])(vs: Rs[V]): Rs[V]
  def in[V, E](g: G[V, E])(vs: Rs[V]): Rs[V]

  def outDegree[V, E](g: G[V, E])(vs: Rs[V]): Int =
    self.out(g)(vs).size

  def inDegree[V, E](g: G[V, E])(vs: Rs[V]): Int =
    self.in(g)(vs).size

  def isEmpty[V, E](g: G[V, E]): Boolean
  def vertices[V, E](g: G[V, E]): Rs[V]

  def addVertex[V, E](g: G[V, E])(v: V): G[V, E]
  def addEdge[V, E](g: G[V, E])(src: V, dst: V, e: E): G[V, E]

  def addVertices[V, E](g: G[V, E])(v: Rs[V]): (Rs[V], G[V, E])

  def removeVertex[V, E](g: G[V, E])(v: V): G[V, E]
  def removeEdge[V, E](g: G[V, E])(src: V, dst: V, e: E): G[V, E]

  def get[V, E](g: G[V, E])(v: V): Option[V]

  def empty[V, E: ClassTag]: G[V, E]

  def dfsFold[V, E, B](g: G[V, E])(b: B, start: V)(f: (B, V) => B): B =
    fold[V, E, B, List](g)(b, start)(f)

  def bfsFold[V, E, B](g: G[V, E])(b: B, start: V)(f: (B, V) => B): B =
    fold[V, E, B, Queue](g)(b, start)(f)

  def fold[V, E, B, M[_]](
    g: G[V, E]
  )(b: B, start: V)(f: (B, V) => B)(implicit TS: TraverseSupport[M]): B = {
    @tailrec
    def loop(q: M[V], b: B, visited: scala.collection.mutable.Set[V]): B = {
      TS.pop(q) match {
        case Some((v, q)) =>
          val newB = f(b, v)
          val newQ = out(g)(Rs(v)).foldLeft(q) { (q, childV) =>
            if (!visited(childV)) {
              visited += childV
              TS.push(q)(childV)
            } else q
          }
          loop(newQ, newB, visited)
        case None =>
          b
      }
    }

    loop(TS.fromIter(List(start)), b, scala.collection.mutable.Set.empty[V])
  }
}

object Graph {
  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /** Summon an instance of [[Graph]] for `G`.
    */
  @inline def apply[G[_, _]](implicit instance: Graph[G]): Graph[G] = instance

  @deprecated("Use graph.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllGraphOps[G[_, _], A, B](
      target: G[A, B]
    )(implicit tc: Graph[G]): AllOps[G, A, B] {
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
    def outDegree(vs: Rs[A]): Int = typeClassInstance.outDegree[A, B](self)(vs)
    def inDegree(vs: Rs[A]): Int = typeClassInstance.inDegree[A, B](self)(vs)
    def isEmpty: Boolean = typeClassInstance.isEmpty[A, B](self)
    def vertices: Rs[A] = typeClassInstance.vertices[A, B](self)
    def addVertex(v: A): G[A, B] = typeClassInstance.addVertex[A, B](self)(v)
    def addEdge(src: A, dst: A, e: B): G[A, B] =
      typeClassInstance.addEdge[A, B](self)(src, dst, e)
    def addVertices(v: Rs[A]): (Rs[A], G[A, B]) =
      typeClassInstance.addVertices[A, B](self)(v)
    def removeVertex(v: A): G[A, B] =
      typeClassInstance.removeVertex[A, B](self)(v)
    def removeEdge(src: A, dst: A, e: B): G[A, B] =
      typeClassInstance.removeEdge[A, B](self)(src, dst, e)
    def get(v: A): Option[A] = typeClassInstance.get[A, B](self)(v)
    def dfsFold[C](b: C, start: A)(f: (C, A) => C): C =
      typeClassInstance.dfsFold[A, B, C](self)(b, start)(f)
    def bfsFold[C](b: C, start: A)(f: (C, A) => C): C =
      typeClassInstance.bfsFold[A, B, C](self)(b, start)(f)
    def fold[C, M[_]](b: C, start: A)(f: (C, A) => C)(implicit
      TS: TraverseSupport[M]
    ): C = typeClassInstance.fold[A, B, C, M](self)(b, start)(f)(TS)
  }
  trait AllOps[G[_, _], A, B] extends Ops[G, A, B]
  trait ToGraphOps extends Serializable {
    implicit def toGraphOps[G[_, _], A, B](
      target: G[A, B]
    )(implicit tc: Graph[G]): Ops[G, A, B] {
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
