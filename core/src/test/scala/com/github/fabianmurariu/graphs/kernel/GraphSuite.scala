package com.github.fabianmurariu.graphs.kernel
import org.scalacheck.Prop._

import Graph.ops._

import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import com.github.fabianmurariu.graphs.data.DirectedGraph
import com.github.fabianmurariu.graphs.data.EntryIndex
import com.github.fabianmurariu.graphs.data.LookupTable

abstract class GraphSuite[G[_, _], V: Arbitrary, E: Arbitrary](implicit
    G: Graph[G]
) extends ScalaCheckSuite {

  property("graph can add all the vertices then get them back") {
    forAll { (vs: Set[V]) =>
      val g = vs.foldLeft(G.empty[V, E]) { (g, v) =>
        g.addVertex(v)
      }

      assertEquals(g.vertices.to(Set), vs)

    }
  }

}

class UnsafeDirectedGraphSuite
    extends GraphSuite[DirectedGraph[
      *,
      *,
      LookupTable.ImmutableLookupTable,
      EntryIndex.ImmutableEntryIndex
    ], String, Int]
