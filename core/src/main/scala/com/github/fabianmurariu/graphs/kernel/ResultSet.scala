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
import scala.collection.Factory

sealed trait ResultSet[F[_], O]:
  def to[C1](factory: Factory[O, C1])(using Applicative[F]): F[C1]
  def toList(using Applicative[F]) = to(List)
  def toVector(using Applicative[F]) = to(Vector)
  def ++(other: ResultSet[F, O]): ResultSet[F, O] =
    (this, other) match {
      case (IterableResultSet(vs), IterableResultSet(vsOther)) =>
        IterableResultSet(vs ++ vsOther)
    }

object ResultSet:
  def fromIter[F[_], O](vs: Iterable[O]): ResultSet[F, O] =
    IterableResultSet(vs)

case class IterableResultSet[F[_], O](vs: Iterable[O]) extends ResultSet[F, O]:
  def to[C1](factory: Factory[O, C1])(using Applicative[F]) =
    Applicative[F].pure(vs.to[C1](factory))
