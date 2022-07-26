package com.github.fabianmurariu.graphs.data.dg

import com.github.fabianmurariu.graphs.kernel.Graph

/** This Graph is unsafe because it leaks internal ids it is mutable and does
  * not support concurrent access
  *
  * @param vTable
  * @param store
  */
case class DirectedGraph[V, E, M[_], G[V, E]](table: M[V], index: G[V, E])

object DirectedGraph {

  def empty[V, E, M[_]: LookupTable, G[_, _]: EntryIndex] =
    new DirectedGraph(LookupTable[M].empty[V], EntryIndex[G].empty[V, E])

  implicit def graph[M[_]: LookupTable, GG[_, _]: EntryIndex]
    : Graph[DirectedGraph[*, *, M, GG]] = new GraphInstance[M, GG]

}

trait AdjacencyStore[E] {
  def vs: IndexedSeq[Int]
  def props: IndexedSeq[E]

  def appendPair(v: Int, e: E): AdjacencyStore[E]

  def foldLeft[B](b: B)(f: (B, Int) => B): B

  def remove(v: Int): AdjacencyStore[E]
}

case class VecStore[E](
  vs: Vector[Int] = Vector.empty,
  props: Vector[E] = Vector.empty
) extends AdjacencyStore[E] {

  override def remove(v: Int): AdjacencyStore[E] = {
    val newVs = vs.filter(_ != v)
    val newProps = props.view.zipWithIndex.collect {
      case (e, vi) if vs(vi) != v => e // remove all edges linked to v
    }.toVector
    VecStore(newVs, newProps)
  }

  override def foldLeft[B](b: B)(f: (B, Int) => B): B =
    vs.foldLeft(b)(f)

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

sealed trait VertexEntry[+V, E]

case object Empty extends VertexEntry[Nothing, Nothing]

case class Entry[V, E](
  id: Int,
  v: V,
  out: AdjacencyStore[E],
  into: AdjacencyStore[E]
) extends VertexEntry[V, E]
