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

import cats.Applicative
import cats.Monad
import cats.Id

final case class ImmutableGraph[V, E](vs: Map[V, Links[V, E]])

final case class Links[V, E](
    out: Map[V, E] = Map.empty[V, E],
    into: Map[V, E] = Map.empty[V, E]
) { self =>
  def addOutEdge(e: E, dst: V): Links[V, E] =
    self.copy(out = out.updated(dst, e))
  def addInEdge(src: V, e: E): Links[V, E] =
    self.copy(into = into.updated(src, e))
}

object ImmutableGraph:
  given genericImmutableGraph[F[_]](using
      S: Support[F],
      F: Monad[F]
  ): Graph[F, ImmutableGraph] with
    extension [V, E](g: ImmutableGraph[V, E])

      def edges(src: V, dst: V): ResultSet[F, (V, E, V)] = ???
      def findV(v: V): ResultSet[F, V] = ???
      def intoE(v: V): ResultSet[F, (E, V)] = ???
      def neighbours(v: V): ResultSet[F, V] = 
        into(v) ++ out(v)
      def neighboursE(v: V): ResultSet[F, (E, V)] = ???
      def out(v: V): ResultSet[F, V] =
        ResultSet.fromIter[F, V](g.vs.view.flatMap { case (_, links) =>
          links.out.keySet
        })

      def into(v: V): ResultSet[F, V] = 
        ResultSet.fromIter[F, V](g.vs.view.flatMap { case (_, links) =>
          links.into.keySet
        })
      def outE(v: V): ResultSet[F, (E, V)] = ???
      def addVertex(v: V): F[ImmutableGraph[V, E]] =
        fromMap(g.vs.updated(v, Links()))

      def addEdge(src: V, e: E, dst: V): F[ImmutableGraph[V, E]] =
        fromMap(
          g.vs
            .updatedWith(src) {
              case Some(links) => Some(links.addOutEdge(e, dst))
              case None        => Some(Links(Map(dst -> e)))
            }
            .updatedWith(dst) {
              case Some(links) => Some(links.addInEdge(src, e))
              case None        => Some(Links(into = Map(src -> e)))
            }
        )

      def vertices: ResultSet[F, V] =
        IterableResultSet(g.vs.keySet)
    def empty[V, E]: F[ImmutableGraph[V, E]] =
      fromMap(Map.empty)

    def fromMap[V, E](m: Map[V, Links[V, E]]): F[ImmutableGraph[V, E]] =
      Applicative[F].pure(ImmutableGraph(m))
