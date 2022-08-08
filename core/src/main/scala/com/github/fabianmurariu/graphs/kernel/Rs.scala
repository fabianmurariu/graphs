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

import scala.collection.Factory
import scala.collection.mutable.Builder
import scala.annotation.tailrec

/** Result Set
  */
sealed trait Rs[O] {
  def to[C1](factory: Factory[O, C1]): C1
  def toList = to(List)
  def toVector = to(Vector)
  def toSet = to(Set)

  def iterator: Iterator[O] = this match {
    case Rs.EmptyResultSet()      => Iterator.empty
    case Rs.IterableResultSet(vs) => vs.iterator
    case Rs.IdResultSet(ids, _, f) =>
      ids.iterator.map(f)
  }

  def map[A](f: O => A): Rs[A] = this match {
    case empty @ Rs.EmptyResultSet() => empty.asInstanceOf[Rs[A]]
    case Rs.IterableResultSet(vs)    => Rs.IterableResultSet(vs.map(f))
    case ids @ Rs.IdResultSet(_, _, _) =>
      Rs.IterableResultSet(ids.to(Vector).map(f))
  }

  def foldLeft[B](b: B)(f: (B, O) => B): B = this match {
    case Rs.IterableResultSet(vs) => vs.foldLeft(b)(f)
    case ids: Rs.IdResultSet[_, O] =>
      ids.vs.map(ids.f).foldLeft(b)(f)
    case _ => b
  }

  def size: Int = this match {
    case Rs.EmptyResultSet()      => 0
    case Rs.IterableResultSet(vs) => vs.size
    case Rs.IdResultSet(vs, _, _) => vs.size
  }

  def ++(other: Rs[O]): Rs[O] =
    (this, other) match {
      case (Rs.IterableResultSet(vs), Rs.IterableResultSet(vsOther)) =>
        Rs.IterableResultSet(vs ++ vsOther)
      case (left: Rs[O], _: Rs.EmptyResultSet[O])  => left
      case (_: Rs.EmptyResultSet[O], right: Rs[O]) => right
      case (ids: Rs.IdResultSet[_, O] @unchecked, right) =>
        Rs.IterableResultSet(ids.to(Vector)) ++ right
      case (left, ids: Rs.IdResultSet[_, O] @unchecked) =>
        left ++ Rs.IterableResultSet(ids.to(Vector))
    }
}

object Rs {

  def apply[O](v: O*): Rs[O] =
    if (v.isEmpty) EmptyResultSet()
    else fromIter(v)

  def fromIter[O](vs: Iterable[O]): Rs[O] =
    IterableResultSet(vs)

  def empty[O]: Rs[O] = EmptyResultSet()

  case class EmptyResultSet[O]() extends Rs[O] {
    def to[C1](factory: Factory[O, C1]) =
      List.empty.to[C1](factory)
  }

  case class IterableResultSet[O](vs: Iterable[O]) extends Rs[O] {
    def to[C1](factory: Factory[O, C1]) =
      vs.to[C1](factory)
  }

  case class IdResultSet[E, O](
    vs: IndexedSeq[Int],
    e: IndexedSeq[E],
    f: Int => O
  ) extends Rs[O] {

    override def to[C1](factory: Factory[O, C1]): C1 = {

      @tailrec
      def loop(
        b: Builder[O, C1],
        iter: Iterator[Int],
        e: IndexedSeq[E],
        i: Int
      ): C1 = {
        if (!iter.hasNext) b.result()
        else {
          val nextV = iter.next()
          val o = f(nextV)
          b += o
          loop(b, iter, e, i + 1)
        }
      }
      loop(factory.newBuilder, vs.iterator, e, 0)
    }

  }

}
