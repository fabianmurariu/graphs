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

package com.github.fabianmurariu.graphs.data

final case class Links[V, E](
    v: V,
    out: Map[V, E] = Map.empty[V, E],
    into: Map[V, E] = Map.empty[V, E]
) { self =>
  def addOutEdge(dst: V, e: E): Links[V, E] =
    self.copy(out = out.updated(dst, e))
  def addInEdge(src: V, e: E): Links[V, E] =
    self.copy(into = into.updated(src, e))

  def removeOut(v: V): Links[V, E] =
    self.copy(out = out - v)

  def removeInto(v: V): Links[V, E] =
    self.copy(into = into - v)
}