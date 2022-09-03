package graphs.jgrapht

import com.github.fabianmurariu.graphs.kernel.Graph
import com.github.fabianmurariu.graphs.kernel.Rs
import scala.jdk.CollectionConverters._
import org.jgrapht.Graphs
import scala.reflect.ClassTag
import graphs.jgrapht.{JGraphT, JEdge}
import org.jgrapht.graph.DefaultDirectedGraph

class JGraphTDirectedGraphInstance extends Graph[JGraphT] {

  override def out[V, E](g: JGraphT[V, E])(vs: Rs[V]): Rs[V] = {
    // println(s"${Graphs.neighborListOf(g.graph, vs.toList.head)} ${g.graph.outgoingEdgesOf(vs.toList.head)}")

    val out = vs
      .map(v => g.graph.outgoingEdgesOf(v).asScala.map(e => Graphs.getOppositeVertex(g.graph, e, v)))
      .foldLeft(Vector.newBuilder[V]) {_ ++= _}
      .result()
    Rs.fromIter(out)
  }

  override def in[V, E](g: JGraphT[V, E])(vs: Rs[V]): Rs[V] = {
    val out = vs
      .map(v => g.graph.incomingEdgesOf(v).asScala.map(e => Graphs.getOppositeVertex(g.graph, e, v)))
      .foldLeft(Vector.newBuilder[V]) {_ ++= _}
      .result()
    Rs.fromIter(out)
  }

  override def isEmpty[V, E](g: JGraphT[V, E]): Boolean = g.graph.vertexSet().isEmpty()

  override def vertices[V, E](g: JGraphT[V, E]): Rs[V] = {
    Rs.fromIter(g.graph.vertexSet().asScala)
  }

  override def addVertex[V, E](g: JGraphT[V, E])(v: V): JGraphT[V, E] = {
    g.graph.addVertex(v)
    g
  }

  override def addEdge[V, E](g: JGraphT[V, E])(src: V, dst: V, e: E): JGraphT[V, E] ={
    g.graph.addEdge(src, dst, new JEdge(e))
    g
  }

  override def addVertices[V, E](g: JGraphT[V, E])(v: Rs[V]): (Rs[V], JGraphT[V, E]) =
    ???

  override def removeVertex[V, E](g: JGraphT[V, E])(v: V): JGraphT[V, E] = {
    g.graph.removeVertex(v)
    g
  }

  override def removeEdge[V, E](g: JGraphT[V, E])(src: V, dst: V, e: E): JGraphT[V, E] =
    ???

  override def get[V, E](g: JGraphT[V, E])(v: V): Option[V] = {
    if (g.graph.containsVertex(v)) Option(v)
    else None
  }

  override def empty[V, E](implicit CT:ClassTag[E]): JGraphT[V, E] = {
    JGraphT[V, E](new DefaultDirectedGraph(classOf[JEdge[E]]))
  }

}
