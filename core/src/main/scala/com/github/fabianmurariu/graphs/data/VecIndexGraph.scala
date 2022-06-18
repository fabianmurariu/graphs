package com.github.fabianmurariu.graphs.data

import com.github.fabianmurariu.graphs.kernel.BaseGraph
import cats.Id
import com.github.fabianmurariu.graphs.kernel.Rs

class VecIndexGraph[V, E](vTable: LookupTable[V], store: NodeIndex[V, E])
    extends BaseGraph[Id, V, E] {

  override def out(v: V): Rs[Id, (V, E)] = ???

  override def into(v: V): Rs[Id, (V, E)] = ???

  override def outV(v: V): Rs[Id, V] = ???

  override def intoV(v: V): Rs[Id, V] = ???

  override def edges(src: V, dst: V): Rs[Id, (V, E)] = ???

  override def isEmpty: Id[Boolean] = ???

  override def contains(v: V): Id[Option[V]] = ???

  override def vertices: Rs[Id, V] = ???

  override def edges: Rs[Id, (V, V, E)] = ???

  override def addEdge(src: V, dst: V, e: E): BaseGraph[Id, V, E] = ???

  override def addVertex(v: V): BaseGraph[Id, V, E] = ???

  override def removeVertex(v: V): BaseGraph[Id, V, E] = ???

  override def removeEdge(src: V, dst: V): BaseGraph[Id, V, E] = ???
}

trait AdjacencyStore[E]

case class VecStore[E](vs: Vector[Int], props: Vector[E])
    extends AdjacencyStore[E]

trait NodeIndex[V, E] {
  def size: Int

  def getVertex(id: Int): VertexEntry[V, E]
}

case class VecNodeIndex[V, E](ves: Vector[VertexEntry[V, E]])
    extends NodeIndex[V, E] {

  override def size: Int = ves.size

  override def getVertex(id: Int): VertexEntry[V, E] = ves(id)

}

trait LookupTable[V] {
  def getId(v: V): Int
}

sealed trait VertexEntry[V, E]

case object Empty extends VertexEntry[Nothing, Nothing]

case class Entry[V, E](
    id: Int,
    v: V,
    out: AdjacencyStore[E],
    into: AdjacencyStore[E]
) extends VertexEntry[V, E]
