package com.github.fabianmurariu.graphs.data.dg.v2

trait GraphStorage[F[_], +V, +E] {
  def addOrUpdateEntry[VV >: V, EE >: E](physicalId: Int, v: VV)(
    f: VertexEntry[VV, EE] => VertexEntry[VV, EE]
  ): F[GraphStorage[F, VV, EE]]

  def entries[VV >: V, EE >: E](
    physicalIds: Iterable[Int]
  ): F[Iterable[VertexEntry[VV, EE]]]
}
