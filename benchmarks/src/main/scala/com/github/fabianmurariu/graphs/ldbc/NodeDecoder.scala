package com.github.fabianmurariu.graphs.ldbc

trait NodeDecoder[T] {

  def decodeLine(line: String): Either[Throwable, T]

  def id(v: T): Long 

}
