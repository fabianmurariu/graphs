package com.github.fabianmurariu.graphs.data.dg.v2
import cats.Monad
import cats.syntax.all.*
import com.github.fabianmurariu.graphs.kernel.ResultSet
import com.github.fabianmurariu.graphs.kernel.ResultSet.Ids
import com.github.fabianmurariu.graphs.kernel.v2.DirectedGraphF as DGF

class DirectedGraph[F[_], +V, +E, VID](
  table: LookupTable[F, VID],
  adjStore: GraphStorage[F, V, E, VID],
  edgeList: => AdjacencyList[E]
)(implicit val F: Monad[F])
    extends DGF[F, V, E, VID] {

  def removeVertex[VV >: V, EE >: E](id: VID): F[DGF[F, VV, EE, VID]] = ???
  def addEdge[VV >: V, EE >: E](
    srcId: VID,
    dstId: VID,
    edge: EE
  ): F[DGF[F, VV, EE, VID]] = {
    for {
      src <- table.unsafeFind(srcId)
      dst <- table.unsafeFind(dstId)
      adj1 <- adjStore.updateEntry(src) {
        case Empty => throw new IllegalStateException
        case ent @ Entry(_, _, out, _) =>
          ent.copy(out = out.appendPair(dst, edge))
      }
      adj2 <- adj1.updateEntry(dst) {
        case Empty => throw new IllegalStateException
        case ent @ Entry(_, _, _, in) =>
          ent.copy(into = in.appendPair(src, edge))
      }
    } yield new DirectedGraph(table, adj2, edgeList)
  }

  def addVertex[B >: V <: VID, EE >: E](v: B): F[DGF[F, B, EE, VID]] = {
    addVertex(v, v)
  }
  def addVertex[VV >: V, EE >: E](
    id: VID,
    label: VV
  ): F[DGF[F, VV, EE, VID]] = {
    for {
      (physicalId, adj) <- adjStore.allocateId
      tbl <- table.lookupOrCreate(id, physicalId)
      adj <- adj.addEntry(
        physicalId,
        id,
        Entry(physicalId, label, edgeList, edgeList)
      )
    } yield new DirectedGraph(tbl, adj, edgeList)
  }

  def out(v: ResultSet[F, VID]): ResultSet[F, VID] = {

    val physicalIds = v match {
      case Ids(ids, _) => ids
      case rs =>
        for {
          vIds <- rs.next
          pIds <- table.findAll(vIds)
        } yield pIds

    }

    val ids: F[Iterable[Int]] = for {
      pIds <- physicalIds
      adjs <- adjStore
        .entries(pIds)
        .map(ves =>
          ves.flatMap {
            case Entry(_, _, out, _) => out.vertexIds
            case _                   => Iterable.empty[Int]
          }
        )
    } yield adjs

    ResultSet.Ids(ids, adjStore.lookupVIDs)

  }
  def in(v: ResultSet[F, VID]): ResultSet[F, VID] = {

    val physicalIds = v match {
      case Ids(ids, _) => ids
      case rs =>
        for {
          vIds <- rs.next
          pIds <- table.findAll(vIds)
        } yield pIds

    }

    val ids: F[Iterable[Int]] = for {
      pIds <- physicalIds
      adjs <- adjStore
        .entries(pIds)
        .map(ves =>
          ves.flatMap {
            case Entry(_, _, _, in) => in.vertexIds
            case _                  => Iterable.empty[Int]
          }
        )
    } yield adjs

    ResultSet.Ids(ids, adjStore.lookupVIDs)

  }
  def neighbours(v: ResultSet[F, VID]): ResultSet[F, VID] = ???

  def outE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)] = ???
  def inE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)] = ???
  def neighboursE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)] = ???

  def get[VV >: V](id: VID): F[Option[VV]] = {
    for {
      physical <- table.find(id)
      label <- adjStore.entries(physical)
    } yield label.headOption match {
      case Some(Entry(_, v, _, _)) => Some(v)
      case _                       => None
    }
  }
  def vertices: ResultSet[F, V] = adjStore.allEntries
}
