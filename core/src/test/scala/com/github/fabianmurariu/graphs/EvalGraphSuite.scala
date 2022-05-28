package com.github.fabianmurariu.graphs

class EvalGraphSuite extends munit.FunSuite {

  test("eval graph POC 1") {
    val ops = QueryOps.pure
    for {
      src <- ops.vertex("a")
      dst <- ops.vertex("b")
      edge <- ops.edge(src, 1, dst)
      vs <- ops.neighbours(src)
    } yield vs

    
  }

}
