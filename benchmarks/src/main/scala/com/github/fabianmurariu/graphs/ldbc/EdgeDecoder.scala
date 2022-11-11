package com.github.fabianmurariu.graphs.ldbc

trait EdgeDecoder[T] { // FIXME: same as NodeDecoder

  def decodeLine(line: String): Either[Throwable, T]

}
