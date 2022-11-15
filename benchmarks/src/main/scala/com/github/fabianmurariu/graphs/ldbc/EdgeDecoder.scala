package com.github.fabianmurariu.graphs.ldbc

trait EdgeDecoder[E] { // FIXME: same as NodeDecoder

  def decodeLine(line: String): Either[Throwable, E]

  def src(e: E): Long
  def dst(e: E): Long

}
