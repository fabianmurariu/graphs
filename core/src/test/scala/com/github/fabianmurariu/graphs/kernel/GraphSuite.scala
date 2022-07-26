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
  property(
    "graph can represent a star graph where one node points to all the others"
  ) {
    forAll { (v: V, vs: Set[V], e: E) =>
      (!vs(v)) ==> {

        val g = vs.foldLeft(G.empty[V, E].addVertex(v)) { (g, dst) =>
          g.addVertex(dst).addEdge(v, dst, e)
        }
        val rs = Rs(v)

        assertEquals(g.out(rs).size, vs.size)
        assertEquals(g.out(rs).toSet, vs)
        assertEquals(g.vertices.toSet, vs + v)
        vs.forall { v =>
          g.in(Rs(v)).size == 1
        }

      }
    }
  }

  property(
    "a hyperconnected graph will have every node pointing to all other nodes"
  ) {
    forAll { (vs: Set[V], e: E) =>
      (vs.nonEmpty) ==> {

        val g1 = vs.foldLeft(G.empty[V, E]) { (g, v) =>
          g.addVertex(v)
        }

        // for each vertex including myself
        val g2 = vs.foldLeft(g1) { (g, v) =>
          vs.foldLeft(g) { (g, dst) =>
            g.addEdge(v, dst, e)
          }
        }

        assertEquals(g2.vertices.toSet, vs)
        vs.forall { v => g2.out(Rs(v)).toSet == vs }

      }
    }
  }

  property(
    "remove one vertex from a graph with one vertex results in the empty graph"
  ) {
    forAll { (v: V) =>
      val g = G.empty[V, E].addVertex(v).removeVertex(v)
      assert(g.isEmpty)
    }
  }

  property(
    "removing the central node of a star graph means all nodes have no inbound and no outbound"
  ) {
    forAll { (v: V, vs: Set[V], e: E) =>
      (!vs(v)) ==> {

        val g = vs
          .foldLeft(G.empty[V, E].addVertex(v)) { (g, dst) =>
            g.addVertex(dst).addEdge(v, dst, e)
          }
          .removeVertex(v)

        vs.foreach { dst =>
          assertEquals(g.out(Rs(dst)).size, 0)
          assertEquals(g.in(Rs(dst)).size, 0)
        }

        assertEquals(g.vertices.toSet, vs)
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
