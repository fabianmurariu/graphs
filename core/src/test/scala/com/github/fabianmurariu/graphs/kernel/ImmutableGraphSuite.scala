package com.github.fabianmurariu.graphs.kernel

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import com.github.fabianmurariu.graphs.kernel.ImmutableGraph
import cats.Id

class ImmutableGraphSuite extends munit.ScalaCheckSuite:
  property("it can insert all vertices and get them back") {
    forAll { (vs: Set[Int]) =>
      val g = Graph[Id, ImmutableGraph].empty[Int, String]

      val gOut = vs.foldLeft(g) { (g, v) => g.addVertex(v) }

      assertEquals(gOut.vertices.to(Vector), vs.toVector)
    }
  }
