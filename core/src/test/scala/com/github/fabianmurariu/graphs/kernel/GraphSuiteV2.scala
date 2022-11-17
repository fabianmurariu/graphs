package com.github.fabianmurariu.graphs.kernel

import org.scalacheck.Prop._
import cats.syntax.all.*
import cats.Monad
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import scala.reflect.ClassTag
import com.github.fabianmurariu.graphs.kernel.v2.DirectedGraphF
import com.github.fabianmurariu.graphs.kernel.v2.DirectedGraphF.Id


class DirectGraphSuiteV2 extends GraphSuiteV2[Long, String] {
  override def empty = DirectedGraphF.immutableSimpleGraph[Long, String]
}

abstract class GraphSuiteV2[V: Arbitrary, E: Arbitrary: ClassTag](
) extends ScalaCheckSuite {
  def empty: DirectedGraphF.DGraph[V, E]

  property("we can get a node back when the graph is not empty") {
    forAll { (vs: Set[V]) =>
      (vs.nonEmpty) ==> {

        val g = empty

        vs.foreach(v => assertEquals(g.get(v), None))

        val g1 = vs.foldLeft(g) { (g, v) =>
          g.addVertex(v)
        }

        vs.foreach(v => assertEquals(g1.get(v), Some(v)))
      }
    }
  }

  property("graph can add all the vertices then get them back") {
    forAll { (vs: Set[V]) =>
      val g = vs.foldLeft(empty) { (g, v) =>
        g.addVertex(v)
      }

      assertEquals(g.vertices.to(Vector).size, vs.size)
      assertEquals(g.vertices.to(Set), vs)

    }
  }

  property("(1)-['a']->(2) is a valid representable graph") {
    forAll { (src: V, e: E, dst: V) =>
      (src != dst) ==> {
        val g = empty.addVertex(src).addVertex(dst).addEdge(src, dst, e)
        assertEquals(g.out(ResultSet.One(src)).to(List), List(dst))
        assertEquals(g.in(ResultSet.One(dst)).to(List), List(src))
      }
    }
  }

  property(
    "graph can represent a star graph where one node points to all the others"
  ) {
    forAll { (v: V, vs: Set[V], e: E) =>
      (!vs(v)) ==> {

        val g = vs.foldLeft(empty.addVertex(v)) { (g, dst) =>
          g.addVertex(dst).addEdge(v, dst, e)
        }
        val rs = ResultSet.One[Id, V](v)

        assertEquals(g.vertices.to(Set), vs + v)
        assertEquals(g.out(rs).to(Set), vs, s"${g.out(rs).to(Set)} != $vs")
        assertEquals(g.out(rs).size, vs.size.toLong)
        vs.forall { v =>
          g.in(ResultSet.One(v)).size == 1
        }

      }
    }
  }

  property(
    "a hyperconnected graph will have every node pointing to all other nodes"
  ) {
    forAll { (vs: Set[V], e: E) =>
      (vs.nonEmpty) ==> {

        val g1 = vs.foldLeft(empty) { (g, v) =>
          g.addVertex(v)
        }

        // for each vertex including myself
        val g2 = vs.foldLeft(g1) { (g, v) =>
          vs.foldLeft(g) { (g, dst) =>
            g.addEdge(v, dst, e)
          }
        }

        assertEquals(g2.vertices.to(Set), vs)
        vs.forall { v => g2.out(ResultSet.One(v)).to(Set) == vs }

      }
    }
  }

  property(
    "remove one vertex from a graph with one vertex results in the empty graph"
  ) {
    forAll { (v: V) =>
      val g = empty.addVertex(v).removeVertex(v)
      assert(g.isEmpty)
    }
  }

  property(
    "removing the central node of a star graph means all nodes have no inbound and no outbound"
  ) {
    forAll { (v: V, vs: Set[V], e: E) =>
      (!vs(v)) ==> {

        val g = vs
          .foldLeft(empty.addVertex(v)) { (g, dst) =>
            g.addVertex(dst).addEdge(v, dst, e)
          }
          .removeVertex(v)

        vs.foreach { dst =>
          assertEquals(g.out(ResultSet.One(dst)).size, 0L)
          assertEquals(g.in(ResultSet.One(dst)).size, 0L)
        }

        assertEquals(g.vertices.to(Set), vs)
      }
    }
  }

  // property(
  //   "load a heap and traverse with bfs should result in the same order".ignore
  // ) {
  //   forAll { (vSet: Set[V], e: E) =>

  //     val vs = vSet.toVector
  //     val tree = GraphSuiteSupport.mkTree(vs, e, empty)

  //     if (vs.nonEmpty) {
  //       val actual: List[V] = tree.bfsFold(List.empty[V], vs.head) { (l, v) =>
  //         v :: l
  //       }

  //       assertEquals(actual.toVector.reverse, vs)
  //     }
  //   }
  // }

}
