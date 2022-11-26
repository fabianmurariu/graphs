package com.github.fabianmurariu.graphs.kernel

import cats.Monad
import cats.syntax.all.*
import com.github.fabianmurariu.graphs.data.dg.*
import com.github.fabianmurariu.graphs.data.dg.GraphStorage.ImmutableGraphStorage
import com.github.fabianmurariu.graphs.data.dg.LookupTable.ImmutableLookupTable

import scala.collection.mutable

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

  def neighbours(v: ResultSet[F, VID], d: Direction): ResultSet[F, VID]
  def out(v: ResultSet[F, VID]): ResultSet[F, VID] = neighbours(v, Direction.OUT)
  def in(v: ResultSet[F, VID]): ResultSet[F, VID] = neighbours(v, Direction.INTO)

  def neighboursE(vs: ResultSet[F, VID], d: Direction): ResultSet[F, (E, VID)]

  def degree(v: VID, d: Direction): F[Long] = neighbours(ResultSet.One(v), d).size

  def isEmpty = vertices.size.map(_ == 0L)

  def get[VV >: V](id: VID): F[Option[VV]]

//  def dfsFold[VV >: V, B](b: B, start: VV)(f: (B, VV) => B): B =
//    fold[VV, B, List](b, start)(f)
//
//  def bfsFold[VV >: V, B](b: B, start: VV)(f: (B, VV) => B): B =
//    fold[VV, B, Queue](b, start)(f)

  type Ctx[M[_], B] = (B, M[VID], mutable.Set[VID])

  def fold[VV >: V, B, M[_]](b: B, start: VID)(
    f: (B, VID) => B
  )(implicit TS: TraverseSupport[M]): F[B] = {
    val ctx: Ctx[M, B] = (b, TS.fromIter(List(start)), mutable.Set.empty[VID])
    F.tailRecM[Ctx[M, B], Ctx[M, B]](ctx) { case (b, q, visited) =>
      TS.pop(q) match {
        case None => F.pure(Right((b, q, visited)))
        case Some((v, newQ)) =>
          F.pure(Left((b, newQ, visited)))
      }
    }.map(_._1)

//    @tailrec
//    def loop(q: M[VID], b: B, visited: scala.collection.mutable.Set[VID]): B = {
//      TS.pop(q) match {
//        case Some((v, q: M[VID])) =>
//          val newB = f(b, v)
//          this.out(ResultSet.One(v)).foldL(q) { (q, childVID) =>
//            if (!visited(childVID)) {
//              visited += childVID
//              F.pure(TS.push(q)(childVID))
//            } else F.pure(q)
//          }
//
//          val newQ = out(g)(Rs(v)).foldLeft(q) { (q, childV) =>
//            if (!visited(childV)) {
//              visited += childV
//              TS.push(q)(childV)
//            } else q
//          }
//          loop(newQ, newB, visited)
//        case None =>
//          b
//      }
//    }
//
//    loop(TS.fromIter(List(start)), b, scala.collection.mutable.Set.empty[V])
  }

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
