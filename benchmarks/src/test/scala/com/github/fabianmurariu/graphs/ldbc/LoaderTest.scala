package com.github.fabianmurariu.graphs.ldbc

import com.github.fabianmurariu.graphs.kernel.DirectedGraphF
import com.github.fabianmurariu.graphs.ldbc.schema.{LdbcEdge, Person}

import java.nio.file.Paths

class LoaderTest extends munit.FunSuite {

  val loader = NodesLoader[Person]

  test("can load Person".ignore) {

    val location = "/sf1/composite-merged-fk/initial_snapshot/dynamic/Person"
    val path = Paths.get(
      this.getClass
        .getResource(location)
        .getPath
    )
    assert(path != null, s"can't find ${location}")

    val graph0 = DirectedGraphF.mutableLabeledGraph64[Person, LdbcEdge]

    loader
      .loadNodes[LdbcEdge](graph0)(path)
      .fold(
        t => throw t,
        { g =>
          assert(g.vertices.size > 0)
        }
      )

  }

}
