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

import com.github.fabianmurariu.graphs.ir.Query
import simulacrum.typeclass
import com.github.fabianmurariu.graphs.ir.Ref
import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an instance of EvalGraph for ${G}")
@typeclass
trait EvalGraph[G[_, _]] extends Serializable {
  def eval[V, E, O <: Ref](g: G[V, E])(q: Query[V, E, O]): Rs[O]
}

object EvalGraph {
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
    def eval[C <: Ref](q: Query[A, B, C]): Rs[C] =
      typeClassInstance.eval[A, B, C](self)(q)
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
