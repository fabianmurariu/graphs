package com.github.fabianmurariu.graphs.data.dg.v2

trait LookupTable[F[_], A] {

  def lookupOrCreate(a: A): F[(Int, LookupTable[F, A])]

  def find(a: A): F[Int]
  def findAll(a: Iterable[A]): F[Iterable[Int]]

  def toLogicalIds(a: Iterable[Int]): F[Iterable[A]]
}
