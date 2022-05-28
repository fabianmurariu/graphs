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

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import com.github.fabianmurariu.graphs.kernel.ImmutableGraph
import cats.Id

class ImmutableGraphSuite extends munit.ScalaCheckSuite {
  property("it can insert all vertices and get them back") {
    forAll { (vs: Set[Int]) =>
      val g = Graph[Id, ImmutableGraph].empty[Int, String]

      val gOut = vs.foldLeft(g) { (g, v) => g.addVertex(v) }

      assertEquals(gOut.vertices.to(Vector), vs.toVector)
    }

  }

  property("it can create a path from a list of nodes") {
    forAll { (vs: Set[Int]) =>
      if (vs.nonEmpty) {

        val g = linkPath[ImmutableGraph, Int](vs)

        assert(g.edges.to(List).nonEmpty)

      }
    }
  }

  test("a graph with a single vertex connected to itself") {
    val g = linkPath[ImmutableGraph, Int](Set(0))
    assertEquals(g.out(0).to(List), List(0))
  }

  test("a graph with a two vertices connected to in a loop") {
    val g = linkPath[ImmutableGraph, Int](Set(0, 1))
    assertEquals(g.out(0).to(List), List(1))
    assertEquals(g.out(1).to(List), List(0))
    assertEquals(g.into(0).to(List), List(1))
    assertEquals(g.into(1).to(List), List(0))
  }

  def linkPath[G[_, _], V](vs: Set[V])(using Graph[Id, G]): G[V, String] = {

    val vec = vs.toVector
    val pairs = vec.zip(vec.tail :+ vec.head)

    val g = pairs.foldLeft(Graph[Id, G].empty[V, String]) {
      case (g, (src, dst)) =>
        g.addEdge(src, s"${src}_${dst}", dst)
    }

    pairs.foreach { case (src, dst) =>
      g.out(src).to(List) == List(dst)
      assertEquals(g.out(src).to(List), List(dst))
      assertEquals(g.into(dst).to(List), List(src))
    }
    g
  }
}
