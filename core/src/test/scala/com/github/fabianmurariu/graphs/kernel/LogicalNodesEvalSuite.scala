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

import com.github.fabianmurariu.graphs.data.dg.DirectedGraph
import com.github.fabianmurariu.graphs.data.dg.LookupTable
import com.github.fabianmurariu.graphs.data.dg.EntryIndex
import com.github.fabianmurariu.graphs.ir.LogicalNode
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import com.github.fabianmurariu.graphs.ir.Node
import org.scalacheck.Arbitrary
import com.github.fabianmurariu.graphs.ir.Ref
import scala.reflect.ClassTag

abstract class LogicalNodesEvalSuite[G[
  _,
  _
]: Graph: EvalGraph, V: Arbitrary, E: ClassTag]
    extends ScalaCheckSuite {

  import LogicalNode._
  property("insert vertex then project".ignore) {

    forAll { (v: V) =>

      val add = new Node[V]
      val root = Project(LNRef(add))
      val table: Map[Ref, AddNodes[V]] =
        Map(add -> AddNodes(Vector(v)))

      val res = EvalGraph[G].eval(Graph[G].empty[V, E])(root, table)
      assertEquals(res, Rs(Map("v_id" -> 0, "v" -> v)))
      println(res)
    }

  }

}

class UnsafeDirectedEvalGraphSuite
    extends LogicalNodesEvalSuite[DirectedGraph[
      LookupTable.ImmutableLookupTable,
      EntryIndex.ImmutableEntryIndex,
      *,
      *
    ], String, Int]
