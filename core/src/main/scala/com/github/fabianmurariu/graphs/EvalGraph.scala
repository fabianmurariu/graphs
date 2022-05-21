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

package com.github.fabianmurariu.graphs

import cats.free.FreeT
import cats.{~>, Applicative}
import cats.Id

sealed trait Query[V, E, O]
final case class Neighbours[V, E](v: V) extends Query[V, E, Iterable[(V, E, V)]]
final case class NewVertex[V, E](v: V) extends Query[V, E, V]
final case class NewEdge[V, E](src: V, e: E, dst: V) extends Query[V, E, E]

type QueryT[M[_], V, E, O] = FreeT[[X] =>> Query[V, E, X], M, O]

trait QueryOps[M[_]: Applicative]:
  def neighbours[V, E](v: V) =
    FreeT.liftF[[X] =>> Query[V, E, X], M, Iterable[(V, E, V)]](Neighbours(v))
  def vertex[V, E](v: V) =
    FreeT.liftF[[X] =>> Query[V, E, X], M, V](NewVertex(v))
  def edge[V, E](src: V, e: E, dst: V) =
    FreeT.liftF[[X] =>> Query[V, E, X], M, E](NewEdge(src, e, dst))

object QueryOps:
  def pure: QueryOps[Id] = new QueryOps[Id] {}

trait EvalGraph[G[_, _]]:
  extension [V, E](g: G[V, E]) def eval[M[_]]: (([X] =>> Query[V, E, X]) ~> M)
