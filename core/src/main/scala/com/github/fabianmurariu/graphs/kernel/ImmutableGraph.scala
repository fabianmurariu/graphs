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

final case class ImmutableGraph[V, E](vs: Map[V, Links[V, E]]) {

  def addVertext(v: V): ImmutableGraph[V, E] = {
    ImmutableGraph(vs.updated(v, Links()))
  }

  def vertices = vs.keySet
}

final case class Links[V, E](
    out: Map[V, E] = Map.empty[V, E],
    into: Map[V, E] = Map.empty[V, E]
)

object ImmutableGraph:
  given genericImmutableGraph[F[_]](using
      S: Support[F],
      F: Monad[F]
  ): Graph[F, ImmutableGraph] with
    extension [V, E](g: ImmutableGraph[V, E])
      def neighbours(v: V): ResultSet[F, (V, E, V)] = ???
      def addVertex(v: V): F[ImmutableGraph[V, E]] = {
        Applicative[F].pure(g.addVertext(v))
      }
      def addEdge(src: V, e: E, dst: V): F[ImmutableGraph[V, E]] = ???
      def vertices: ResultSet[F, V] =
        IterableResultSet(g.vertices)
      def edge(src: V, dst: V): ResultSet[F, (V, E, V)] = ???
    def empty[V, E]: F[ImmutableGraph[V, E]] =
      Applicative[F].pure(new ImmutableGraph(Map.empty))
