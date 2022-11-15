package com.github.fabianmurariu.graphs.data.dg.v2

sealed trait VertexEntry[+V, +E]

case object Empty extends VertexEntry[Nothing, Nothing]

case class Entry[+V, +E](
  id: Int,
  v: V,
  out: AdjacencyList[E],
  into: AdjacencyList[E]
) extends VertexEntry[V, E]

