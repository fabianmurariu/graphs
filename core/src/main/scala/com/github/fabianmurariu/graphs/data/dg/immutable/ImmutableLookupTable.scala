package com.github.fabianmurariu.graphs.data.dg.immutable

import com.github.fabianmurariu.graphs.data.dg.LookupTable

final case class ImmutableLookupTable[V](
  table: Map[V, Int] = Map.empty,
  max: Int = 0,
  free: Set[Int] = Set.empty
)

object ImmutableLookupTable {

  class LookupTableInstance extends LookupTable[ImmutableLookupTable] {

    override def lookup[A](m: ImmutableLookupTable[A])(a: A): Option[Int] =
      m.table.get(a)

    override def update[A](
      m: ImmutableLookupTable[A]
    )(a: A): (Int, ImmutableLookupTable[A]) = {
      m.table.get(a) match {
        case Some(value) => (value, m)
        case None =>
          if (m.free.isEmpty) {
            m.max -> m.copy(max = m.max + 1, table = m.table + (a -> m.max))
          } else {
            val id = m.free.head
            id -> m.copy(free = m.free.tail, table = m.table + (a -> id))
          }
      }
    }

    override def remove[A](
      m: ImmutableLookupTable[A]
    )(a: A): (Option[Int], ImmutableLookupTable[A]) = {
      m.table.get(a) match {
        case None => None -> m
        case ret @ Some(id) =>
          ret -> m.copy(free = m.free + id, table = m.table - a)
      }
    }

    override def isEmpty[A](m: ImmutableLookupTable[A]): Boolean =
      m.table.isEmpty

    override def empty[A]: ImmutableLookupTable[A] =
      ImmutableLookupTable(Map.empty, 0, Set.empty)

  }

}
