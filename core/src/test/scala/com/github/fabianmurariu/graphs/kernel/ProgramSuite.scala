package com.github.fabianmurariu.graphs.kernel

import com.github.fabianmurariu.graphs.data.dg.DirectedGraph
import com.github.fabianmurariu.graphs.syntax.toGraphOps

class ProgramSuite extends munit.FunSuite {

  test("step into SSSP on a diamond graph") {

    val p: SSSP[String,Long] = new SSSP[String, Long]()

    val g = DirectedGraph.default[String, Long]

    // two paths
    // a -5-> b1 -7-> c
    // a -2-> b2 -3-> c

    val graph = g.addVertex("a")
      .addVertex("b1")
      .addVertex("b2")
      .addVertex("c")
      .addEdge("a", "b1", 5L)
      .addEdge("a", "b2", 2L)
      .addEdge("b1", "c", 7L)
      .addEdge("b2", "c", 3L)


    val ctx = Program.run(p)(graph, Seed.Vertex("a", 0L))
    println(ctx)

  }

}
