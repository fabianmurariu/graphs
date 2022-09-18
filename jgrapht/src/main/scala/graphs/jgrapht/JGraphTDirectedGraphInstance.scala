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

package graphs.jgrapht

import com.github.fabianmurariu.graphs.kernel.Graph
import com.github.fabianmurariu.graphs.kernel.Rs
import scala.jdk.CollectionConverters._
import org.jgrapht.Graphs
import scala.reflect.ClassTag
import graphs.jgrapht.{JEdge, JGraphT}
import org.jgrapht.graph.DefaultDirectedGraph

class JGraphTDirectedGraphInstance extends Graph[JGraphT] {

  override def outE[V, E](g: JGraphT[V,E])(vs: Rs[V]): Rs[(E, V)] = ???

  override def inE[V, E](g: JGraphT[V,E])(vs: Rs[V]): Rs[(E, V)] = ???


  override def out[V, E](g: JGraphT[V, E])(vs: Rs[V]): Rs[V] = {
    val out = vs
      .map(v =>
        g.graph
          .outgoingEdgesOf(v)
          .asScala
          .map(e => Graphs.getOppositeVertex(g.graph, e, v))
      )
      .foldLeft(Vector.newBuilder[V]) { _ ++= _ }
      .result()
    Rs.fromIter(out)
  }

  override def in[V, E](g: JGraphT[V, E])(vs: Rs[V]): Rs[V] = {
    val out = vs
      .map(v =>
        g.graph
          .incomingEdgesOf(v)
          .asScala
          .map(e => Graphs.getOppositeVertex(g.graph, e, v))
      )
      .foldLeft(Vector.newBuilder[V]) { _ ++= _ }
      .result()
    Rs.fromIter(out)
  }

  override def isEmpty[V, E](g: JGraphT[V, E]): Boolean =
    g.graph.vertexSet().isEmpty()

  override def vertices[V, E](g: JGraphT[V, E]): Rs[V] = {
    Rs.fromIter(g.graph.vertexSet().asScala)
  }

  override def addVertex[V, E](g: JGraphT[V, E])(v: V): JGraphT[V, E] = {
    g.graph.addVertex(v)
    g
  }

  override def addEdge[V, E](
    g: JGraphT[V, E]
  )(src: V, dst: V, e: E): JGraphT[V, E] = {
    g.graph.addEdge(src, dst, new JEdge(e))
    g
  }

  override def addVertices[V, E](g: JGraphT[V, E])(
    v: Rs[V]
  ): (Rs[V], JGraphT[V, E]) =
    ???

  override def removeVertex[V, E](g: JGraphT[V, E])(v: V): JGraphT[V, E] = {
    g.graph.removeVertex(v)
    g
  }

  override def removeEdge[V, E](
    g: JGraphT[V, E]
  )(src: V, dst: V, e: E): JGraphT[V, E] =
    ???

  override def get[V, E](g: JGraphT[V, E])(v: V): Option[V] = {
    if (g.graph.containsVertex(v)) Option(v)
    else None
  }

  override def empty[V, E](implicit CT: ClassTag[E]): JGraphT[V, E] = {
    JGraphT[V, E](new DefaultDirectedGraph(classOf[JEdge[E]]))
  }

}
