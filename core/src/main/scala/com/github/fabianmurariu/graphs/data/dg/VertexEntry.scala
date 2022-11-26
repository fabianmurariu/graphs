package com.github.fabianmurariu.graphs.data.dg

sealed trait VertexEntry[+V, +E, VID]

object VertexEntry{
  def empty[V, E, VID]: VertexEntry[V, E, VID] = Empty.asInstanceOf[VertexEntry[V, E, VID]]
}
case object Empty extends VertexEntry[Nothing, Nothing, Nothing]

case class Entry[+V, +E, VID](
  vid: VID,
  v: V,
  out: AdjacencyList[E],
  into: AdjacencyList[E]
) extends VertexEntry[V, E, VID]

