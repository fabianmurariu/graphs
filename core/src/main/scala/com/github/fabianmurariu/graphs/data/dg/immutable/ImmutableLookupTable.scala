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
