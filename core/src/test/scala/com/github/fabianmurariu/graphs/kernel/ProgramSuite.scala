package com.github.fabianmurariu.graphs.kernel

import com.github.fabianmurariu.graphs.data.dg.DirectedGraph
import com.github.fabianmurariu.graphs.syntax.toGraphOps

class ProgramSuite extends munit.FunSuite {

  test("step into SSSP on a diamond graph") {

    val p: SSSP[String,Int] = new SSSP[String, Int]()

    val g = DirectedGraph.default[String, Int]

    // two paths
    // a -5-> b1 -7-> c
    // a -2-> b2 -3-> c

    val graph = g.addVertex("a")
      .addVertex("b1")
      .addVertex("b2")
      .addVertex("c")
      .addEdge("a", "b1", 5)
      .addEdge("a", "b2", 2)
      .addEdge("b1", "c", 7)
      .addEdge("b2", "c", 3)


    Program.run(p)(graph, Seed.Vertex("a", 0L))

  }

}
