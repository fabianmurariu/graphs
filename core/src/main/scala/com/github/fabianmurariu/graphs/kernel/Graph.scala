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

import cats.Id

trait Graph[F[_], G[_, _]]:
  extension [V, E](g: G[V, E])
    def out(v: V): ResultSet[F, V]
    def into(v: V): ResultSet[F, V]
    def neighbours(v: V): ResultSet[F, V]

    def outE(v: V): ResultSet[F, (E, V)]
    def intoE(v: V): ResultSet[F, (E, V)]
    def neighboursE(v: V): ResultSet[F, (E, V)]

    def findV(v: V): ResultSet[F, V]
    def edges(src: V, dst: V): ResultSet[F, (V, E, V)]
    def vertices: ResultSet[F, V]

    def addVertex(v: V): F[G[V, E]]
    def addEdge(src: V, e: E, dst: V): F[G[V, E]]

  def empty[V, E]: F[G[V, E]]

object Graph:
  def apply[F[_], G[_, _]](using g: Graph[F, G]): Graph[F, G] = g

trait Support[F[_]]

object Support:
  given nopeId: Support[Id] = new Support[Id] {}
