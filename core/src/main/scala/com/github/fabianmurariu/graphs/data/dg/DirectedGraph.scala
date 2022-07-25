package com.github.fabianmurariu.graphs.data.dg

import com.github.fabianmurariu.graphs.kernel.Rs
import com.github.fabianmurariu.graphs.kernel.Rs.IdResultSet
import com.github.fabianmurariu.graphs.syntax._
import com.github.fabianmurariu.graphs.kernel.Graph

/** This Graph is unsafe because it leaks internal ids it is mutable and does
  * not support concurrent access
  *
  * @param vTable
  * @param store
  */
case class DirectedGraph[V, E, M[_]: LookupTable, G[V, E]: EntryIndex](
    table: M[V],
    index: G[V, E]
) { self =>

  def out(vs: Rs[V]): Rs[(V, E)] = vs match {
    case IdResultSet(vs, _, _) =>
      vs.map { v =>
        index.entry(v) match {
          case Entry(_, _, out, _) =>
            IdResultSet(
              out.vs,
              out.props,
              { i: Int =>
                index.entry(i) match {
                  case Entry(_, v, _, _) =>
                    v -> out.props(i)
                }
              }
            )
          case _ => Rs.empty[(V, E)]
        }
      }.reduce(_ ++ _)
  }

  def into(v: V): Rs[(V, E)] = ???

  def outV(v: V): Rs[V] = {
    index.entry(table.lookup(v)) match {
      case Entry(_, _, out, _) =>
        IdResultSet(
          out.vs,
          out.props,
          { i: Int =>
            index.entry(i) match {
              case Entry(_, v, _, _) =>
                v
            }
          }
        )
      case _ => Rs.empty[V]
    }

  }

  def intoV(v: V): Rs[V] = ???

  def edges(src: V, dst: V): Rs[(V, E)] = ???

  def isEmpty: Boolean = ???

  def contains(v: V): Option[V] = ???

  def vertices: Rs[V] = ???

  def addEdge(src: V, dst: V, e: E): DirectedGraph[V, E, M, G] = ???

  def removeVertex(v: V): DirectedGraph[V, E, M, G] = ???

  def removeEdge(src: V, dst: V): DirectedGraph[V, E, M, G] = ???
}

object DirectedGraph {

  def empty[V, E, M[_]: LookupTable, G[_, _]: EntryIndex] =
    new DirectedGraph(
      LookupTable[M].empty[V],
      EntryIndex[G].empty[V, E]
    )

  implicit def graph[M[_]: LookupTable, GG[_, _]: EntryIndex]
      : Graph[DirectedGraph[*, *, M, GG]] = new GraphInstance[M, GG]

}

trait AdjacencyStore[E] {
  def vs: IndexedSeq[Int]
  def props: IndexedSeq[E]

  def appendPair(v: Int, e: E): AdjacencyStore[E]
}

case class VecStore[E](
    vs: Vector[Int] = Vector.empty,
    props: Vector[E] = Vector.empty
) extends AdjacencyStore[E] {

  override def appendPair(v: Int, e: E): AdjacencyStore[E] =
    this.copy(vs = vs :+ v, props :+ e)

}

trait NodeIndex[V, E] {
  def size: Int

  def getVertex(id: Int): VertexEntry[V, E]
}

case class VecNodeIndex[V, E](ves: Vector[VertexEntry[V, E]])
    extends NodeIndex[V, E] {

  def size: Int = ves.size

  def getVertex(id: Int): VertexEntry[V, E] = ves(id)

}

sealed trait VertexEntry[V, E]

case object Empty extends VertexEntry[Nothing, Nothing]

case class Entry[V, E](
    id: Int,
    v: V,
    out: AdjacencyStore[E],
    into: AdjacencyStore[E]
) extends VertexEntry[V, E]
