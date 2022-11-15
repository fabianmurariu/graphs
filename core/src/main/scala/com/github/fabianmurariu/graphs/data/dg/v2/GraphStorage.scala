package com.github.fabianmurariu.graphs.data.dg.v2

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

}
