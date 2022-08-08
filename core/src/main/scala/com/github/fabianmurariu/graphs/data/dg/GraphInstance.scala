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

package com.github.fabianmurariu.graphs.data.dg

import com.github.fabianmurariu.graphs.kernel.{Graph, GraphError}
import com.github.fabianmurariu.graphs.kernel.Rs
import com.github.fabianmurariu.graphs.kernel.Rs.EmptyResultSet
import com.github.fabianmurariu.graphs.kernel.Rs.IterableResultSet
import com.github.fabianmurariu.graphs.syntax._
import scala.collection.mutable.ReusableBuilder
import scala.annotation.tailrec

class GraphInstance[M[_]: LookupTable, GG[_, _]: EntryIndex]
    extends Graph[DirectedGraph[*, *, M, GG]] {

  override def in[V, E](g: DirectedGraph[V, E, M, GG])(vs: Rs[V]): Rs[V] =
    vs match {
      case Rs.IdResultSet(vs, _, _) =>
        vs.map { intoVId(g) }.reduce(_ ++ _)
      case e @ EmptyResultSet() => e
      case IterableResultSet(vs) =>
        vs.flatMap(g.table.lookup(_))
          .map { intoVId(g) }
          .reduce(_ ++ _)
    }
  override def out[V, E](g: DirectedGraph[V, E, M, GG])(vs: Rs[V]): Rs[V] =
    vs match {
      case Rs.IdResultSet(vs, _, _) =>
        vs.map { outVId(g) }.reduce(_ ++ _)
      case e @ EmptyResultSet() => e
      case IterableResultSet(vs) =>
        vs.flatMap(g.table.lookup(_))
          .map { outVId(g) }
          .reduce(_ ++ _)
    }

  private def intoVId[V, E](g: DirectedGraph[V, E, M, GG])(vId: Int): Rs[V] = {
    g.index.entry(vId) match {
      case Entry(_, _, _, into) =>
        adjStoreToRs(g)(into)
      case _ => Rs.empty[V]
    }
  }

  private def outVId[V, E](g: DirectedGraph[V, E, M, GG])(vId: Int): Rs[V] = {
    g.index.entry(vId) match {
      case Entry(_, _, out, _) =>
        adjStoreToRs(g)(out)
      case _ => Rs.empty[V]
    }
  }

  private def adjStoreToRs[V, E](
    g: DirectedGraph[V, E, M, GG]
  )(as: AdjacencyStore[E]): Rs[V] = {
    Rs.IdResultSet(
      as.vs,
      as.props,
      { i: Int =>
        g.index.entry(i) match {
          case Entry(_, v, _, _) => v
        }
      }
    )
  }

  override def isEmpty[V, E](g: DirectedGraph[V, E, M, GG]): Boolean =
    g.table.isEmpty

  override def vertices[V, E](g: DirectedGraph[V, E, M, GG]): Rs[V] =
    Rs.fromIter(g.index.vertices)

  override def addVertex[V, E](
    g: DirectedGraph[V, E, M, GG]
  )(v: V): DirectedGraph[V, E, M, GG] = {
    val (vId, newTable) = g.table.update(v)
    val newStore = g.index.addOrUpdateEntry(vId, v)(identity)
    new DirectedGraph(newTable, newStore)
  }

  override def addVertices[V, E](
    g: DirectedGraph[V, E, M, GG]
  )(vs: Rs[V]): (Rs[V], DirectedGraph[V, E, M, GG]) = {

    @tailrec
    def loop(
      iter: Iterator[V],
      b: scala.collection.mutable.Builder[V, Vector[V]],
      g: DirectedGraph[V, E, M, GG]
    ): DirectedGraph[V, E, M, GG] = {
      if (iter.hasNext) {
        val v = iter.next()
        val g1 = addVertex(g)(v)
        b += v
        loop(iter, b, g1)
      } else g
    }
    val b = Vector.newBuilder[V]
    val g1 = loop(vs.iterator, b, g)
    Rs.fromIter(b.result()) -> g1
  }

  override def addEdges[V, E](g: DirectedGraph[V, E, M, GG])(
    src: Rs[V],
    dst: Rs[V],
    e: Rs[E]
  ): Either[GraphError, DirectedGraph[V, E, M, GG]] = {
    Left(GraphError.AddEdgeFailed("boom!"))
  }

  override def addEdge[V, E](
    g: DirectedGraph[V, E, M, GG]
  )(src: V, dst: V, e: E): DirectedGraph[V, E, M, GG] = {

    val index = for {
      srcId <- g.table.lookup(src)
      dstId <- g.table.lookup(dst)
    } yield g.index
      .addOrUpdateEntry(srcId, src) {
        case Empty =>
          Entry(srcId, src, VecStore(Vector(dstId), Vector(e)), VecStore())
        case ent @ Entry(_, _, outStore, _) =>
          ent.copy(out = outStore.appendPair(dstId, e))
      }
      .addOrUpdateEntry(dstId, dst) {
        case Empty =>
          Entry(dstId, dst, VecStore(), VecStore(Vector(srcId), Vector(e)))
        case ent @ Entry(_, _, _, intoStore) =>
          ent.copy(into = intoStore.appendPair(srcId, e))
      }

    new DirectedGraph(g.table, index.getOrElse(g.index))
  }

  override def removeVertex[V, E](
    g: DirectedGraph[V, E, M, GG]
  )(v: V): DirectedGraph[V, E, M, GG] = {
    g.table.remove(v) match {
      case (None, _) => g
      case (Some(id), table) =>
        val index = g.index.removeVertex(id)
        new DirectedGraph(table, index)
    }
  }

  override def removeEdge[V, E](
    g: DirectedGraph[V, E, M, GG]
  )(src: V, dst: V, e: E): DirectedGraph[V, E, M, GG] = ???

  override def get[V, E](g: DirectedGraph[V, E, M, GG])(v: V): Option[V] =
    g.table
      .lookup(v)
      .map(g.index.entry)
      .flatMap {
        case Entry(_, v, _, _) => Some(v)
        case _                 => None
      }

  override def empty[V, E]: DirectedGraph[V, E, M, GG] =
    new DirectedGraph[V, E, M, GG](LookupTable[M].empty, EntryIndex[GG].empty)
}
