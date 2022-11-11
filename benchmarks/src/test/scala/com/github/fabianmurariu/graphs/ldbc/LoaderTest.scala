package com.github.fabianmurariu.graphs.ldbc

import com.github.fabianmurariu.graphs.data.dg.DirectedGraph
import com.github.fabianmurariu.graphs.ldbc.schema.{LdbcEdge, Person}
import com.github.fabianmurariu.graphs.syntax.*

import java.nio.file.Paths
import java.time.format.DateTimeFormatter

class LoaderTest extends munit.FunSuite {

  val loader = NodesLoader[Person]

  test("what date are you") {
    val d = "2011-06-11T14:18:48.094+00:00"

    DateTimeFormatter.ISO_ZONED_DATE_TIME.parse(d)
  }

  test("can load Person") {

    val location = "/sf1/composite-merged-fk/initial_snapshot/dynamic/Person"
    val path = Paths.get(
      this.getClass
        .getResource(location)
        .getPath
    )
    assert(path != null, s"can't find ${location}")

    val g = loader.loadNodes(DirectedGraph.default[Person, LdbcEdge])(path)

    assert(g.vertices.size > 0)

  }

}
