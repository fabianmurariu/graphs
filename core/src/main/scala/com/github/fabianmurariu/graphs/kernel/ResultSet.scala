package com.github.fabianmurariu.graphs.kernel

import cats.Applicative
import scala.collection.Factory

sealed trait ResultSet[F[_], O]:
  def to[C1](factory: Factory[O, C1]): F[C1]
  def toList = to(List)
  def toVector = to(Vector)

case class IterableResultSet[F[_]: Applicative, O](vs: Iterable[O])
    extends ResultSet[F, O]:
  def to[C1](factory: Factory[O, C1]) = Applicative[F].pure(vs.to[C1](factory))
