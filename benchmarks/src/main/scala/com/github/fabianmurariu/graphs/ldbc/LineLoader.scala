package com.github.fabianmurariu.graphs.ldbc
import java.nio.file.Path
import java.nio.file.Files

import com.github.fabianmurariu.graphs.kernel.v2.DirectedGraphF.LabeledDGraph64
import scala.jdk.CollectionConverters.IteratorHasAsScala



trait LineLoader {

  def loadLines[B](dir: Path)(b: B)(f: (B, String) => B): B = {

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
      .foldLeft(b) {f}

  }
}

abstract class EdgesLoader[E](implicit ED: EdgeDecoder[E]) extends LineLoader {

  def loadEdges[V](
    g: LabeledDGraph64[V, E]
  )(dir: Path): Either[Throwable, LabeledDGraph64[V, E]] = {

    loadLines(dir)(Right(g).withLeft[Throwable]) { (g, line) =>
      for {
        e <- ED.decodeLine(line)
        graph <- g
      } yield {
        val src = ED.src(e)
        val dst = ED.src(e)
        graph.addEdge(src, dst, e)
      }

    }

  }
}

object EdgesLoader {
    def apply[E:EdgeDecoder] = new EdgesLoader[E] {}
}

abstract class NodesLoader[V](implicit ND:NodeDecoder[V]) extends LineLoader {

    def loadNodes[E](g: LabeledDGraph64[V, E])(dir: Path): Either[Throwable, LabeledDGraph64[V, E]] = {

    loadLines(dir)(Right(g).withLeft[Throwable]) { (g, line) =>
      for {
        v <- ND.decodeLine(line)
        graph <- g
      } yield {
        val id = ND.id(v)
        graph.addVertex(id, v)
      }

    }

  }
}

object NodesLoader {
    def apply[V:NodeDecoder] = new NodesLoader[V] {}
}