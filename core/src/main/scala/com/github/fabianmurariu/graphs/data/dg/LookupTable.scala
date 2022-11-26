package com.github.fabianmurariu.graphs.data.dg

import cats.Monad

trait LookupTable[F[_], A] {
  def lookupOrCreate(a: A): F[(Int, LookupTable[F, A])]
  def find(a: A): F[Option[Int]]
  def unsafeFind(a: A): F[Int]
  def findAll(a: Iterable[A]): F[Iterable[Int]]

  def remove(a: A): F[(Option[Int], LookupTable[F, A])]

}

object LookupTable {
  case class ImmutableLookupTable[F[_], A](
    tbl: Map[A, Int] = Map.empty[A, Int],
    emptySlots: List[Int] = List.empty,
    physical: Int = 0
  )(implicit val F: Monad[F])
      extends LookupTable[F, A] {
    override def lookupOrCreate(a: A): F[(Int, LookupTable[F, A])] =
      F.pure {
        tbl.get(a) match {
          case Some(i) => i -> this
          case None =>
            emptySlots match {
              case Nil =>
                val newId = physical
                newId -> ImmutableLookupTable(
                  tbl = tbl.updated(a, newId),
                  physical = newId + 1,
                  emptySlots = emptySlots
                )
              case empty :: rest =>
                empty -> ImmutableLookupTable(
                  tbl = tbl.updated(a, empty),
                  physical = physical,
                  emptySlots = rest
                )
            }
        }
      }

    override def find(a: A): F[Option[Int]] = F.pure(tbl.get(a))
    override def unsafeFind(a: A): F[Int] = F.pure(tbl(a))

    override def findAll(a: Iterable[A]): F[Iterable[Int]] =
      F.pure(a.flatMap(tbl.get))

    override def remove(a: A): F[(Option[Int], LookupTable[F, A])] = F.pure {
      tbl.get(a) match {
        case None     => None -> this
        case Some(id) => Option(id) -> ImmutableLookupTable(tbl.removed(a))
      }
    }

  }
}
