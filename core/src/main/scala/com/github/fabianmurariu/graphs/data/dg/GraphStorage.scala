package com.github.fabianmurariu.graphs.data.dg

import cats.Monad
import com.github.fabianmurariu.graphs.kernel.ResultSet

trait GraphStorage[F[_], +V, +E, VID] {
  def remove[VV >: V, EE >: E](
    physicalId: Option[Int]
  ): F[GraphStorage[F, VV, EE, VID]]

  def addEntry[VV >: V, EE >: E](
    physicalId: Int,
    vId: VID,
    ve: VertexEntry[VV, EE, VID]
  ): F[GraphStorage[F, VV, EE, VID]]

  def updateEntry[VV >: V, EE >: E](physicalId: Int)(
    f: VertexEntry[VV, EE, VID] => VertexEntry[VV, EE, VID]
  ): F[GraphStorage[F, VV, EE, VID]]
  def entries[VV >: V, EE >: E](
    physicalIds: Iterable[Int]
  ): F[Iterable[VertexEntry[VV, EE, VID]]]

  def lookupVIDs(vIds: Iterable[Int]): F[Iterable[VID]]

  def allEntries: ResultSet[F, V]

}

object GraphStorage {
  case class ImmutableGraphStorage[F[_], +V, +E, VID](
    entries: Vector[VertexEntry[V, E, VID]] = Vector.empty
  )(implicit F: Monad[F])
      extends GraphStorage[F, V, E, VID] {

    override def allEntries: ResultSet[F, V] =
      ResultSet.PureRs[F, V](F.pure(entries.collect { case Entry(_, v, _, _) =>
        v
      }))

    override def addEntry[VV >: V, EE >: E](
      physicalId: Int,
      vId: VID,
      ve: VertexEntry[VV, EE, VID]
    ): F[GraphStorage[F, VV, EE, VID]] = F.pure {
      if (entries.size == physicalId) {
        this.copy(entries = entries :+ ve)
      } else {
        this.copy(entries = entries.updated(physicalId, ve))
      }
    }

    override def updateEntry[VV >: V, EE >: E](physicalId: Int)(
      f: VertexEntry[VV, EE, VID] => VertexEntry[VV, EE, VID]
    ): F[GraphStorage[F, VV, EE, VID]] = F.pure {
      this.copy(entries = entries.updated(physicalId, f(entries(physicalId))))
    }

    override def entries[VV >: V, EE >: E](
      physicalIds: Iterable[Int]
    ): F[Iterable[VertexEntry[VV, EE, VID]]] = F.pure {
      physicalIds.map(entries)
    }

    override def remove[VV >: V, EE >: E](
      physicalId: Option[Int]
    ): F[GraphStorage[F, VV, EE, VID]] = F.pure {
      physicalId match {
        case None => this
        case Some(id) =>
          entries(id) match {
            case Entry(_, _, out, into) =>
              val newEntries = entries.updated(id, VertexEntry.empty)

              val es0 = out.vertexIds.foldLeft(newEntries) { (es, i) =>
                es.updated(
                  i,
                  entries(i) match {
                    case e @ Entry(_, _, _, into) =>
                      e.copy(into = into.remove(id))
                  }
                )
              }

              val es1 = into.vertexIds.foldLeft(es0) { (es, i) =>
                es.updated(
                  i,
                  entries(i) match {
                    case e @ Entry(_, _, out, _) =>
                      e.copy(into = out.remove(id))
                  }
                )
              }
              this.copy(entries = es1)
            case _ => this
          }
      }
    }

    override def lookupVIDs(vIds: Iterable[Int]): F[Iterable[VID]] = F.pure {
      vIds
        .map { id =>
          entries(id)
        }
        .collect { case e: Entry[V, E, VID] => e.vid }
    }
  }
}
