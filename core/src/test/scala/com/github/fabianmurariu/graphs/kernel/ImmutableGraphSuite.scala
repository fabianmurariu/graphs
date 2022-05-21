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
