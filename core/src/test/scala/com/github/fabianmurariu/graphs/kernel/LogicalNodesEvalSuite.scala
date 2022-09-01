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

abstract class LogicalNodesEvalSuite[G[_, _]: Graph: EvalGraph, V: Arbitrary, E:ClassTag]
    extends ScalaCheckSuite {

  import LogicalNode._
  property("insert vertex then project") {

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
      *,
      *,
      LookupTable.ImmutableLookupTable,
      EntryIndex.ImmutableEntryIndex
    ], String, Int]
