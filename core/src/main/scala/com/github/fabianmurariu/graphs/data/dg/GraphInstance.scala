package com.github.fabianmurariu.graphs.data.dg

import com.github.fabianmurariu.graphs.kernel.Graph
import com.github.fabianmurariu.graphs.kernel.Rs
import com.github.fabianmurariu.graphs.syntax._

class GraphInstance[M[_]: LookupTable, GG[_, _]: EntryIndex]
    extends Graph[DirectedGraph[*, *, M, GG]] {

  override def out[V, E](g: DirectedGraph[V, E, M, GG])(vs: Rs[V]): Rs[V] = ???

  override def in[V, E](g: DirectedGraph[V, E, M, GG])(vs: Rs[V]): Rs[V] = ???

  override def isEmpty[V, E](g: DirectedGraph[V, E, M, GG]): Boolean = ???

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
  )(src: V, dst: V, e: E): DirectedGraph[V, E, M, GG] = ???

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
