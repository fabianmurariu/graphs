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
/**
  * Result Set
  */
sealed trait Rs[F[_], O] {
  def to[C1](factory: Factory[O, C1])(implicit F: Applicative[F]): F[C1]
  def toList(implicit F: Applicative[F]) = to(List)
  def toVector(implicit F: Applicative[F]) = to(Vector)
  def toSet(implicit F: Applicative[F]) = to(Set)

  def map[A](f: O => A): Rs[F, A] = this match {
    case empty @ EmptyResultSet() => empty.asInstanceOf[Rs[F, A]]
    case IterableResultSet(vs)    => IterableResultSet(vs.map(f))
  }

  def size(implicit F:Applicative[F]): F[Int] = this match {
    case EmptyResultSet() => F.pure(0)
    case IterableResultSet(vs)    => F.pure(vs.size)
  }

  def ++(other: Rs[F, O]): Rs[F, O] =
    (this, other) match {
      case (IterableResultSet(vs), IterableResultSet(vsOther)) =>
        IterableResultSet(vs ++ vsOther)
      case (left: IterableResultSet[F, O], _: EmptyResultSet[F, O])  => left
      case (_: EmptyResultSet[F, O], right: IterableResultSet[F, O]) => right
      case (_: EmptyResultSet[F, O], empty: EmptyResultSet[F, O])    => empty
    }
}

object Rs {
  def fromIter[F[_], O](vs: Iterable[O]): Rs[F, O] =
    IterableResultSet(vs)
}

case class EmptyResultSet[F[_], O]() extends Rs[F, O] {
  def to[C1](factory: Factory[O, C1])(implicit F: Applicative[F]) =
    Applicative[F].pure(List.empty.to[C1](factory))
}

case class IterableResultSet[F[_], O](vs: Iterable[O]) extends Rs[F, O] {
  def to[C1](factory: Factory[O, C1])(implicit F: Applicative[F]) =
    Applicative[F].pure(vs.to[C1](factory))
}
