package com.github.fabianmurariu.graphs.data.dg.v2

import cats.Monad

trait LookupTable[F[_], A] {

  def lookupOrCreate(a: A, physical: Int): F[LookupTable[F, A]]

  def find(a: A): F[Int]
  def findAll(a: Iterable[A]): F[Iterable[Int]]

}

object LookupTable {
  case class ImmutableLookupTable[F[_], A](tbl: Map[A, Int])(implicit
    val F: Monad[F]
  ) extends LookupTable[F, A] {
    override def lookupOrCreate(a: A, physical: Int): F[LookupTable[F, A]] =
      F.pure {
        ImmutableLookupTable(tbl.updatedWith(a) {
          case None =>
            Some(physical)
          case any => any
        })
      }

    override def find(a: A): F[Int] = F.pure(tbl(a))

    override def findAll(a: Iterable[A]): F[Iterable[Int]] =
      F.pure(a.flatMap(tbl.get))

  }
}
