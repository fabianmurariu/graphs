package com.github.fabianmurariu.graphs.ldbc

import com.github.fabianmurariu.graphs.kernel.Graph

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import com.github.fabianmurariu.graphs.syntax.*

abstract class NodesLoader[T](implicit ND: NodeDecoder[T]) {

  def loadNodes[G[_, _]: Graph, E](g: G[T, E])(dir: Path): G[T, E] = {

    Files
      .newDirectoryStream(dir)
      .iterator()
      .asScala
      .flatMap { path =>
        val br = Files.newBufferedReader(path) // FIXME: this doesn't close
        br.lines()
          .iterator()
          .asScala
      }
      .map(ND.decodeLine)
      .tapEach {
        case Left(t) => t.printStackTrace()
        case _       =>
      }
      .collect { case Right(n) => n }
      .foldLeft(g) { (g, n) =>
        g.addVertex(n)
      }

  }

}

object NodesLoader {
  def apply[T: NodeDecoder]: NodesLoader[T] = new NodesLoader[T] {}
}
