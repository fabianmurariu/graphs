package com.github.fabianmurariu.graphs.kernel

sealed abstract class GraphError(msg: String, t: Throwable)
    extends RuntimeException(msg, t) {}

object GraphError {
  case class AddEdgeFailed(msg: String) extends GraphError(msg, null)
}
