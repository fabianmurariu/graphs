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

import simulacrum.typeclass
import com.github.fabianmurariu.graphs.ir.Ref
import scala.annotation.implicitNotFound
import com.github.fabianmurariu.graphs.ir.LogicalNode
@implicitNotFound("Could not find an instance of EvalGraph for ${G}")
@typeclass
trait EvalGraph[G[_, _]] extends Serializable {
  def eval[V, E](
    g: G[V, E]
  )(start: LogicalNode, table: Map[Ref, LogicalNode]): Rs[Map[String, Any]]
}

object EvalGraph {

  class DefaultEvalGraphForGraph[G[_, _]: Graph] extends EvalGraph[G] {

    override def eval[V, E](
      g: G[V, E]
    )(ln: LogicalNode, table: Map[Ref, LogicalNode]): Rs[Map[String, Any]] = {

      // find the Ret key
      // match the select logical node
      // start exploring the tree in a DFS fashion

      val b = Vector.newBuilder[Map[String, Any]]
      evalNode(g)(ln, table) { rb => }
      Rs.fromIter(b.result())

    }

    private def evalNode[V, E](
      g: G[V, E]
    )(ln: LogicalNode, exprs: Map[Ref, LogicalNode])(
      cb: RecordBatch[V, E] => Unit
    ): Unit = ln match {
      case sel: LogicalNode.Project => evalSelect(g)(sel, exprs)(cb)
      case add: LogicalNode.AddNodes[V] @unchecked =>
        evalAddNode(add)(g)(cb)
    }

    case class RecordBatch[V, E](fields: Map[Ref, Vector[Any]], g: G[V, E]) {
      def col(i: Ref): Vector[Any] = fields(i)
      def numCols: Int = fields.size

      def project(refs: Set[Ref]): RecordBatch[V, E] =
        RecordBatch(fields.filter { case (key, _) => refs(key) }, g)
    }

    def evalSelect[V, E](
      g: G[V, E]
    )(ln: LogicalNode.Project, exprs: Map[Ref, LogicalNode])(
      cb: RecordBatch[V, E] => Unit
    ): Unit = ln match {
      case LogicalNode.Project(LogicalNode.LNRef(ref)) =>
        val c = exprs(ref)
        evalNode(g)(c, exprs) { rb =>
          cb(rb.project(Set(ref)))
        }
    }

    def evalAddNode[V, E](
      ln: LogicalNode.AddNodes[V]
    )(g: G[V, E])(cb: RecordBatch[V, E] => Unit) = {
      // cb(RecordBatch(Map(ln.ref -> col.toVector), g1))
      ???
    }
  }

  implicit def evalGraphForGraph[G[_, _]: Graph]: DefaultEvalGraphForGraph[G] =
    new DefaultEvalGraphForGraph[G]

  /* ======================================================================== */
  /* THE FOLLOWING CODE IS MANAGED BY SIMULACRUM; PLEASE DO NOT EDIT!!!!      */
  /* ======================================================================== */

  /** Summon an instance of [[EvalGraph]] for `G`.
    */
  @inline def apply[G[_, _]](implicit instance: EvalGraph[G]): EvalGraph[G] =
    instance

  @deprecated("Use graph.syntax object imports", "2.2.0")
  object ops {
    implicit def toAllEvalGraphOps[G[_, _], A, B](
      target: G[A, B]
    )(implicit tc: EvalGraph[G]): AllOps[G, A, B] {
      type TypeClassType = EvalGraph[G]
    } = new AllOps[G, A, B] {
      type TypeClassType = EvalGraph[G]
      val self: G[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  trait Ops[G[_, _], A, B] extends Serializable {
    type TypeClassType <: EvalGraph[G]
    def self: G[A, B]
    val typeClassInstance: TypeClassType
    def eval(
      start: LogicalNode,
      table: Map[Ref, LogicalNode]
    ): Rs[Map[String, Any]] = typeClassInstance.eval[A, B](self)(start, table)
  }
  trait AllOps[G[_, _], A, B] extends Ops[G, A, B]
  trait ToEvalGraphOps extends Serializable {
    implicit def toEvalGraphOps[G[_, _], A, B](
      target: G[A, B]
    )(implicit tc: EvalGraph[G]): Ops[G, A, B] {
      type TypeClassType = EvalGraph[G]
    } = new Ops[G, A, B] {
      type TypeClassType = EvalGraph[G]
      val self: G[A, B] = target
      val typeClassInstance: TypeClassType = tc
    }
  }
  @deprecated("Use graph.syntax object imports", "2.2.0")
  object nonInheritedOps extends ToEvalGraphOps

  /* ======================================================================== */
  /* END OF SIMULACRUM-MANAGED CODE                                           */
  /* ======================================================================== */

}
