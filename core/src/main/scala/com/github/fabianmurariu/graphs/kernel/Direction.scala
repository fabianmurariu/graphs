package com.github.fabianmurariu.graphs.kernel

sealed trait Direction

object Direction{
  case object OUT extends Direction
  case object INTO extends Direction
  case object BOTH extends Direction
}
