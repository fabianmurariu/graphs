package com.github.fabianmurariu.graphs.kernel

import com.github.fabianmurariu.graphs.data.dg.DirectedGraph
import com.github.fabianmurariu.graphs.data.dg.LookupTable
import com.github.fabianmurariu.graphs.data.dg.EntryIndex
import com.github.fabianmurariu.graphs.ir.LogicalNode
import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import com.github.fabianmurariu.graphs.ir.Query
import com.github.fabianmurariu.graphs.ir.Node
import org.scalacheck.Arbitrary
import com.github.fabianmurariu.graphs.ir.Row

class EvalGraphSuite[G[_, _]: Graph: EvalGraph, V: Arbitrary, E]
    extends ScalaCheckSuite {

  property("insert vertex") {

    import Query._

    forAll { (v: V) =>

      val q = for {
        add <- Query.AddVertex[V, E](Vector(v))
      } yield Vector(add)

      val res = EvalGraph[G].eval(Graph[G].empty[V, E])(q)
    }

  }

}

class UnsafeDirectedEvalGraphSuite
    extends EvalGraphSuite[DirectedGraph[
      *,
      *,
      LookupTable.ImmutableLookupTable,
      EntryIndex.ImmutableEntryIndex
    ], String, Int]
