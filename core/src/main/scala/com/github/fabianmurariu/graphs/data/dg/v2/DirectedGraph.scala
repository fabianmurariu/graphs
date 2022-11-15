package com.github.fabianmurariu.graphs.data.dg.v2
import com.github.fabianmurariu.graphs.kernel.v2.{DirectedGraphF => DGF}
import cats.Monad
import cats.syntax.all.*
import com.github.fabianmurariu.graphs.kernel.ResultSet
import com.github.fabianmurariu.graphs.kernel.ResultSet.Ids

class DirectedGraph[F[_], +V, +E, VID](
  table: LookupTable[F, VID],
  adjStore: GraphStorage[F, V, E],
  edgeList: => AdjacencyList[E]
)(implicit val F: Monad[F])
    extends DGF[F, V, E, VID] {

  def removeVertex[VV >: V, EE >: E](id: VID): F[
    com.github.fabianmurariu.graphs.kernel.v2.DirectedGraphF[F, VV, EE, VID]
  ] = ???
  def addEdge[VV >: V, EE >: E](
    srcId: VID,
    dstId: VID,
    edge: EE
  ): F[DGF[F, VV, EE, VID]] = {
    for {
      src <- table.find(srcId)
      dst <- table.find(srcId)
      adj1 <- adjStore.addOrUpdateEntry(src, srcId) {
        case Empty => throw new IllegalStateException
        case ent @ Entry(_, _, out, _) =>
          ent.copy(out = out.appendPair(dst, edge))
      }
      adj2 <- adj1.addOrUpdateEntry(dst, dstId) {
        case Empty => throw new IllegalStateException
        case ent @ Entry(_, _, _, in) =>
          ent.copy(into = in.appendPair(dst, edge))
      }
    } yield new DirectedGraph(
      table,
      adj2.asInstanceOf[GraphStorage[F, V, E]],
      edgeList
    )
  }

  def addVertex[VV >: V, EE >: E](id: VID): F[DGF[F, VV, EE, VID]] = {
    ???
  }
  def addVertex[VV >: V, EE >: E](
    id: VID,
    label: VV
  ): F[DGF[F, VV, EE, VID]] = {
    for {
      (physicalId, tbl) <- table.lookupOrCreate(id)
      adj <- adjStore.addOrUpdateEntry(physicalId, label) {
        case Empty => Entry(physicalId, label, edgeList, edgeList)
        case ve    => ve
      }
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

    val f: F[Iterable[Int]] = for {
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

    ResultSet.Ids(f, table.toLogicalIds)

  }
  def in(v: ResultSet[F, VID]): ResultSet[F, VID] = ???
  def neighbours(v: ResultSet[F, VID]): ResultSet[F, VID] = ???

  def outE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)] = ???
  def inE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)] = ???
  def neighboursE(vs: ResultSet[F, VID]): ResultSet[F, (E, VID)] = ???

  def unsafeGetVertex[VV >: V](id: VID): F[VV] = ???
  def vertices: ResultSet[F, V] = ???
}
