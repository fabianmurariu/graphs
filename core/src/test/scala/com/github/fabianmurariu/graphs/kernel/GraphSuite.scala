/*
 * Copyright 2022 32Bytes Software LTD
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.fabianmurariu.graphs.kernel
import org.scalacheck.Prop._

import com.github.fabianmurariu.graphs.syntax._
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import com.github.fabianmurariu.graphs.data.dg._
import scala.reflect.ClassTag

abstract class GraphSuite[G[_, _], V: Arbitrary, E: Arbitrary: ClassTag](implicit
  G: Graph[G]
) extends ScalaCheckSuite {

  property("we can get a node back when the graph is not empty") {
    forAll { (vs: Set[V]) =>
      (vs.nonEmpty) ==> {

        val g = G.empty[V, E]

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

  test("wha!".only) {
    val g = Vector(1, 2, 3).foldLeft(G.empty[Int, String])(_ addVertex _)
    // val g = G.empty[Int, String].addVertex(Rs.fromIter(Vector(1, 2, 3)))
    g.addEdge(2, 1, "one")
    g.addEdge(2, 3, "one")

    val rs = Rs(2)
    assertEquals(g.out(rs).toVector, Vector(1, 3))
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

        assertEquals(g.vertices.toSet, vs + v)
        assertEquals(g.out(rs).toSet, vs, s"${g.out(rs).toSet} != $vs" )
        assertEquals(g.out(rs).size, vs.size)
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

  property(
    "load a heap and traverse with bfs should result in the same order"
  ) {
    forAll { (vSet: Set[V], e: E) =>

      val vs = vSet.toVector
      val tree = GraphSuiteSupport.mkTree(vs, e, Graph[G].empty[V, E])

      if (vs.nonEmpty) {
        val actual: List[V] = tree.bfsFold(List.empty[V], vs.head) { (l, v) =>
          v :: l
        }

        assertEquals(actual.toVector.reverse, vs)
      }
    }
  }

}

abstract class GraphSuiteNumeric[G[_, _], V: Arbitrary: Numeric, E: Arbitrary: ClassTag](
  implicit G: Graph[G]
) extends ScalaCheckSuite {

  property("can traverse and sum all the nodes in a path graph") {
    forAll { (vs: Set[V], e: E) =>
      vs.nonEmpty ==> {

        val actual = vs
          .to(Vector)
          .sliding(2)
          .foldLeft(G.empty[V, E]) {
            case (g, s) if s.size == 2 =>
              val src = s.head
              val dst = s.tail.head
              g.addVertex(src).addVertex(dst).addEdge(src, dst, e)
            case (g, s) if s.size == 1 =>
              g.addVertex(s.head)
            case (g, _) => g
          }
          .dfsFold(Numeric[V].zero, vs.head)(Numeric[V].plus)
        assertEquals(actual, vs.sum)
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

class UnsafeDirectedGraphSuiteNum
    extends GraphSuiteNumeric[DirectedGraph[
      *,
      *,
      LookupTable.ImmutableLookupTable,
      EntryIndex.ImmutableEntryIndex
    ], Int, Int]

object GraphSuiteSupport {
  def mkTree[G[_, _]: Graph, V, E](
    vs: Vector[V],
    e: E,
    g: G[V, E],
    i: Int = 0
  ): G[V, E] = {
    if (i < vs.length) {
      val v = vs(i)
      val g1 = g.addVertex(v)
      // add the left node
      val left = 2 * i + 1
      val g2: G[V, E] = if (left < vs.length) {
        // we added left let's add an edge
        mkTree(vs, e, g1, left).addEdge(v, vs(left), e)
      } else mkTree(vs, e, g1, left)

      // add the right node
      val right = 2 * i + 2
      val g3 = if (right < vs.length) {
        // we added right let's add an edge
        mkTree(vs, e, g2, right).addEdge(v, vs(right), e)
      } else mkTree(vs, e, g2, right)
      g3
    } else g
  }
}
