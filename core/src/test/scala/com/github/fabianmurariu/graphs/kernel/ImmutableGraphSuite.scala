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

      vs.foreach { v => assert(gOut.contains(v)) }
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

  property("a vector removed is not pointed at by anyone") {
    forAll { (vs: Set[Int]) =>
      if (vs.nonEmpty) {

        val g = linkPath[ImmutableGraph, Int](vs).removeVertex(vs.head)

        val vertices = g.edges.toSet.flatMap { case (v1, _, v2) => Set(v1, v2) }

        assertEquals(g.vertices.toSet, vs - vs.head)
        assert(!vertices(vs.head))
      }
    }
  }

  property("single edge graph remove vertex is empty") {
    forAll { (i: Int) =>
      val emptyGraph = Graph[Id, ImmutableGraph].empty[Int, String]
      assert(emptyGraph.isEmpty)
      val g = emptyGraph.addVertex(i).removeVertex(i)
      assertEquals(g, emptyGraph)
      assert(g.isEmpty)
    }
  }

  property(
    "removing center vertex in a star graph leaves it with no edges only vertices"
  ) {
    forAll { (vs: Set[Int]) =>
      if (vs.nonEmpty) {
        val g =
          linkStart[ImmutableGraph, Int](vs.head, vs.tail).removeVertex(vs.head)
        assertEquals(g.edges.toSet, Set())
        assertEquals(g.vertices.toSet, vs.tail)
      }
    }
  }

  property(
    "in a star graph the center vertex out neighbours are all the other nodes"
  ) {
    forAll { (vs: Set[Int]) =>
      if (vs.nonEmpty) {
        val c = vs.head
        val rest = vs.tail

        val g =
          linkStart[ImmutableGraph, Int](c, rest)
        assertEquals(g.out(c).toSet, rest)

        if (rest.nonEmpty)
          assertEquals(rest.flatMap(v => g.into(v).toSet), Set(c))

        assertEquals(g.vertices.toSet, vs)
      }
    }
  }

  test("remove vertex") {
    val vs = Set(0, 1, -1)

    val g = linkPath[ImmutableGraph, Int](vs).removeVertex(0)

    val vertices = g.edges.toSet.flatMap { case (v1, _, v2) => Set(v1, v2) }

    assertEquals(g.vertices.toSet, Set(1, -1))
    assert(!vertices(0))
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
        g.addVertex(src).addVertex(dst).addEdge(src, s"${src}_${dst}", dst)
    }

    pairs.foreach { case (src, dst) =>
      g.out(src).to(List) == List(dst)
      assertEquals(g.out(src).to(List), List(dst))
      assertEquals(g.into(dst).to(List), List(src))
    }
    g
  }

  def linkStart[G[_, _], V](first: V, rest: Set[V])(using
      Graph[Id, G]
  ): G[V, String] = {

    val g = rest.foldLeft(Graph[Id, G].empty[V, String].addVertex(first)) {
      (g, dst) =>
        g.addVertex(dst).addEdge(first, s"${first}_${dst}", dst)
    }

    g
  }
}
