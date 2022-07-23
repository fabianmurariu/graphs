package com.github.fabianmurariu.graphs.kernel

import com.github.fabianmurariu.graphs.ir.Query
import simulacrum.typeclass
import com.github.fabianmurariu.graphs.ir.Ref

@typeclass
trait EvalGraph[G[_, _]] {
  def eval[V, E, O <: Ref](g: G[V, E])(q: Query[V, E, O]): Rs[O]
}
