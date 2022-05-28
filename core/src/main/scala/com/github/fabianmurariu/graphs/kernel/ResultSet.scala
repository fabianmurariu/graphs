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
import cats.Functor

sealed trait ResultSet[F[_], O]:
  def to[C1](factory: Factory[O, C1])(using Applicative[F]): F[C1]
  def toList(using Applicative[F]) = to(List)
  def toVector(using Applicative[F]) = to(Vector)
  def toSet(using Applicative[F]) = to(Set)

  def map[A](f: O => A): ResultSet[F, A] = this match {
    case empty@EmptyResultSet() => empty.asInstanceOf[ResultSet[F, A]]
    case IterableResultSet(vs) => IterableResultSet(vs.map(f))
  }

  def ++(other: ResultSet[F, O]): ResultSet[F, O] =
    (this, other) match {
      case (IterableResultSet(vs), IterableResultSet(vsOther)) =>
        IterableResultSet(vs ++ vsOther)
      case (left: IterableResultSet[F, O], _: EmptyResultSet[F, O])  => left
      case (_: EmptyResultSet[F, O], right: IterableResultSet[F, O]) => right
      case (_: EmptyResultSet[F, O], empty: EmptyResultSet[F, O])    => empty
    }

object ResultSet:
  def fromIter[F[_], O](vs: Iterable[O]): ResultSet[F, O] =
    IterableResultSet(vs)

case class EmptyResultSet[F[_], O]() extends ResultSet[F, O] {
  def to[C1](factory: Factory[O, C1])(using Applicative[F]) =
    Applicative[F].pure(List.empty.to[C1](factory))
}

case class IterableResultSet[F[_], O](vs: Iterable[O]) extends ResultSet[F, O]:
  def to[C1](factory: Factory[O, C1])(using Applicative[F]) =
    Applicative[F].pure(vs.to[C1](factory))

  
