package com.github.fabianmurariu.graphs.data.dg

import com.github.fabianmurariu.graphs.kernel.Graph
import com.github.fabianmurariu.graphs.kernel.Rs
import com.github.fabianmurariu.graphs.syntax._
import com.github.fabianmurariu.graphs.kernel.Rs.EmptyResultSet
import com.github.fabianmurariu.graphs.kernel.Rs.IterableResultSet

class GraphInstance[M[_]: LookupTable, GG[_, _]: EntryIndex]
    extends Graph[DirectedGraph[*, *, M, GG]] {

  override def in[V, E](g: DirectedGraph[V, E, M, GG])(vs: Rs[V]): Rs[V] =
    vs match {
      case Rs.IdResultSet(vs, _, _) =>
        vs.map { intoVId(g) }.reduce(_ ++ _)
      case e @ EmptyResultSet() => e
      case IterableResultSet(vs) =>
        vs.map(g.table.lookup(_))
          .map { intoVId(g) }
          .reduce(_ ++ _)
    }
  override def out[V, E](g: DirectedGraph[V, E, M, GG])(vs: Rs[V]): Rs[V] =
    vs match {
      case Rs.IdResultSet(vs, _, _) =>
        vs.map { outVId(g) }.reduce(_ ++ _)
      case e @ EmptyResultSet() => e
      case IterableResultSet(vs) =>
        vs.map(g.table.lookup(_))
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

  override def isEmpty[V, E](g: DirectedGraph[V, E, M, GG]): Boolean = g.table.isEmpty

  override def vertices[V, E](g: DirectedGraph[V, E, M, GG]): Rs[V] =
    Rs.fromIter(g.index.vertices)

  override def addVertex[V, E](g: DirectedGraph[V, E, M, GG])(
      v: V
  ): DirectedGraph[V, E, M, GG] = {
    val (vId, newTable) = g.table.update(v)
    val newStore = g.index.addOrUpdateEntry(vId, v)(identity)
    new DirectedGraph(newTable, newStore)
  }
  override def addEdge[V, E](
      g: DirectedGraph[V, E, M, GG]
  )(src: V, dst: V, e: E): DirectedGraph[V, E, M, GG] = {
    val srcId = g.table.lookup(src)
    val dstId = g.table.lookup(dst)

    val index = g.index
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

    new DirectedGraph(g.table, index)
  }

  override def removeVertex[V, E](g: DirectedGraph[V, E, M, GG])(
      v: V
  ): DirectedGraph[V, E, M, GG] = ???

  override def removeEdge[V, E](
      g: DirectedGraph[V, E, M, GG]
  )(src: V, dst: V, e: E): DirectedGraph[V, E, M, GG] = ???

  override def get[V, E](g: DirectedGraph[V, E, M, GG])(v: V): Option[V] = ???

  override def empty[V, E]: DirectedGraph[V, E, M, GG] =
    new DirectedGraph[V, E, M, GG](
      LookupTable[M].empty,
      EntryIndex[GG].empty
    )
}
