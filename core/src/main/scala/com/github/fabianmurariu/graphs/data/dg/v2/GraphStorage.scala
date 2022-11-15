package com.github.fabianmurariu.graphs.data.dg.v2

import cats.Monad
import com.github.fabianmurariu.graphs.kernel.ResultSet

trait GraphStorage[F[_], +V, +E, VID] {
  def addEntry[VV >: V, EE >: E](
    physicalId: Int,
    vId: VID,
    ve: VertexEntry[VV, EE]
  ): F[GraphStorage[F, VV, EE, VID]]

  def updateEntry[VV >: V, EE >: E](physicalId: Int)(
    f: VertexEntry[VV, EE] => VertexEntry[VV, EE]
  ): F[GraphStorage[F, VV, EE, VID]]
  def entries[VV >: V, EE >: E](
    physicalIds: Iterable[Int]
  ): F[Iterable[VertexEntry[VV, EE]]]

  def allocateId[VV >: V, EE >: E]: F[(Int, GraphStorage[F, VV, EE, VID])]

  def lookupVIDs(vIds: Iterable[Int]): F[Iterable[VID]]

  def allEntries: ResultSet[F, V]

}

object GraphStorage {
  case class ImmutableGraphStorage[F[_], +V, +E, VID](
    entries: Vector[VertexEntry[V, E]] = Vector.empty,
    log2Phys: Vector[VID] = Vector.empty,
    emptySlots: List[Int] = List.empty
  )(implicit F: Monad[F])
      extends GraphStorage[F, V, E, VID] {

    override def allEntries: ResultSet[F, V] =
      ResultSet.PureRs[F, V](F.pure(entries.collect { case Entry(_, v, _, _) => v }))

    override def addEntry[VV >: V, EE >: E](
      physicalId: Int,
      vId: VID,
      ve: VertexEntry[VV, EE]
    ): F[GraphStorage[F, VV, EE, VID]] = F.pure {
      if (entries.size == physicalId) {
        this.copy(entries = entries :+ ve, log2Phys = log2Phys :+ vId)
      } else {
        this.copy(
          entries = entries.updated(physicalId, ve),
          log2Phys = log2Phys.updated(physicalId, vId)
        )
      }
    }

    override def updateEntry[VV >: V, EE >: E](physicalId: Int)(
      f: VertexEntry[VV, EE] => VertexEntry[VV, EE]
    ): F[GraphStorage[F, VV, EE, VID]] = F.pure {
      this.copy(entries = entries.updated(physicalId, f(entries(physicalId))))
    }

    override def entries[VV >: V, EE >: E](
      physicalIds: Iterable[Int]
    ): F[Iterable[VertexEntry[VV, EE]]] = F.pure {
      physicalIds.map(entries)
    }

    override def allocateId[VV >: V, EE >: E]
      : F[(Int, GraphStorage[F, VV, EE, VID])] = {
      F.pure {
        emptySlots match {
          case empty :: rest =>
            empty -> this.copy(emptySlots = rest)
          case _ => // there are no empty slots
            val next = log2Phys.size
            next -> this // TODO: check if this is sound
        }
      }
    }

    override def lookupVIDs(vIds: Iterable[Int]): F[Iterable[VID]] =
      F.pure {
        vIds.map(log2Phys)
      }
  }
}
