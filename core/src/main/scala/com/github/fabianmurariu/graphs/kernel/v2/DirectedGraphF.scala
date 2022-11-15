package com.github.fabianmurariu.graphs.kernel.v2

import cats.Monad
import cats.syntax.all.*
import com.github.fabianmurariu.graphs.data.dg.v2.GraphStorage.ImmutableGraphStorage
import com.github.fabianmurariu.graphs.data.dg.v2.LookupTable.ImmutableLookupTable
import com.github.fabianmurariu.graphs.kernel.ResultSet
import com.github.fabianmurariu.graphs.data.dg.v2.{
  AdjacencyList,
  DirectedGraph,
  GraphStorage,
  LookupTable,
  VecStore
}

/** An effectful graph with [[V]] as vertex label with [[E]] as edge label and
  * [[VID]] as identifier
  */
trait DirectedGraphF[F[_], +V, +E, VID] {

  implicit def F: Monad[F]

  def vertices: ResultSet[F, V]

  def removeVertex[VV >: V, EE >: E](id: VID): F[DirectedGraphF[F, VV, EE, VID]]

  def addVertex[B >: V <: VID, EE >: E](id: B): F[DirectedGraphF[F, B, EE, VID]]

  def addVertex[VV >: V, EE >: E](
    id: VID,
    label: VV
  ): F[DirectedGraphF[F, VV, EE, VID]]

  def addEdge[VV >: V, EE >: E](
    srcId: VID,
    dstId: VID,
    e: EE
  ): F[DirectedGraphF[F, VV, EE, VID]]

  def neighbours(v: ResultSet[F, VID]): ResultSet[F, VID]
  def out(v: ResultSet[F, VID]): ResultSet[F, VID]
  def in(v: ResultSet[F, VID]): ResultSet[F, VID]

  def neighboursE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)]
  def outE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)]
  def inE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)]

  def degree(v: VID): F[Long] = neighbours(ResultSet.One(v)).size

  def isEmpty = vertices.size.map(_ == 0L)

  def get[VV >: V](id: VID): F[Option[VV]]
}

object DirectedGraphF {
  type Id[A] = A

  /** Graph with external index and effect
    */
  type LabeledDGraphF[F[_], V, E] = DirectedGraphF[F, V, E, Int]
  type LabeledDGraph[V, E] = DirectedGraphF[Id, V, E, Int]
  type LabeledDGraph64[+V, +E] = DirectedGraphF[Id, V, E, Long]
  type DGraphF[F[_], V, E] = DirectedGraphF[F, V, E, V]
  type DGraph[V, E] = DirectedGraphF[Id, V, E, V]

  def mutableLabeledGraph64[V, E]: LabeledDGraph64[V, E] = ???

  def apply[F[_]: Monad, V, E, VID](
    table: LookupTable[F, VID],
    store: GraphStorage[F, V, E, VID],
    adjListBuilder: => AdjacencyList[E]
  ): DirectedGraph[F, V, E, VID] = {
    new DirectedGraph[F, V, E, VID](table, store, adjListBuilder)
  }

  def immutableSimpleGraph[V, E]: DirectedGraph[Id, V, E, V] = {
    new DirectedGraph[Id, V, E, V](
      ImmutableLookupTable[Id, V](),
      ImmutableGraphStorage[Id, V, E, V](),
      VecStore[E]()
    )
  }
}
