package com.github.fabianmurariu.graphs.ldbc

import com.github.fabianmurariu.graphs.kernel.Graph

import java.nio.file.Path

abstract class EdgesLoader[T] {

  def loadNodes[G[_, _]: Graph, E](g: G[T, E])(dir: Path): G[T, E] = {

  }
}
