package com.github.fabianmurariu.graphs.kernel
import org.scalacheck.Prop._

import com.github.fabianmurariu.graphs.syntax._
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import com.github.fabianmurariu.graphs.data.dg._

abstract class GraphSuite[G[_, _], V: Arbitrary, E: Arbitrary](implicit
    G: Graph[G]
) extends ScalaCheckSuite {

  property("graph can add all the vertices then get them back") {
    forAll { (vs: Set[V]) =>
      val g = vs.foldLeft(G.empty[V, E]) { (g, v) =>
        g.addVertex(v)
      }

      assertEquals(g.vertices.to(Vector).size, vs.size)
      assertEquals(g.vertices.to(Set), vs)

    }
  }

  property("(1)-['a']->(2) is a valid representable graph") {
    forAll { (src: V, e: E, dst: V) =>
      (src != dst) ==> {
        val g = G.empty[V, E].addVertex(src).addVertex(dst).addEdge(src, dst, e)
        assertEquals(g.out(Rs.fromIter(List(src))).to(List), List(dst))
        assertEquals(g.in(Rs.fromIter(List(dst))).to(List), List(src))
      }
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
