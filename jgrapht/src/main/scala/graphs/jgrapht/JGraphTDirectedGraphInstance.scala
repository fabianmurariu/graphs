package graphs.jgrapht

import com.github.fabianmurariu.graphs.kernel.Graph
import org.jgrapht.graph.{DefaultDirectedGraph => DDG}
import com.github.fabianmurariu.graphs.kernel.Rs
import scala.jdk.CollectionConverters._
import org.jgrapht.Graphs
import scala.reflect.ClassTag

class JGraphTDirectedGraphInstance extends Graph[DDG] {

  override def out[V, E](g: DDG[V, E])(vs: Rs[V]): Rs[V] = {
    println(s"${Graphs.neighborListOf(g, vs.toList.head)}")
    val out = vs
      .map(v => g.outgoingEdgesOf(v).asScala.map(e => Graphs.getOppositeVertex(g, e, v)))
      .foldLeft(Vector.newBuilder[V]) {_ ++= _}
      .result()
    Rs.fromIter(out)
  }

  override def in[V, E](g: DDG[V, E])(vs: Rs[V]): Rs[V] = {
    val out = vs
      .map(v => g.incomingEdgesOf(v).asScala.map(e => Graphs.getOppositeVertex(g, e, v)))
      .foldLeft(Vector.newBuilder[V]) {_ ++= _}
      .result()
    Rs.fromIter(out)
  }

  override def isEmpty[V, E](g: DDG[V, E]): Boolean = g.vertexSet().isEmpty()

  override def vertices[V, E](g: DDG[V, E]): Rs[V] = {
    Rs.fromIter(g.vertexSet().asScala)
  }

  override def addVertex[V, E](g: DDG[V, E])(v: V): DDG[V, E] = {
    g.addVertex(v)
    g
  }

  override def addEdge[V, E](g: DDG[V, E])(src: V, dst: V, e: E): DDG[V, E] ={
    g.addEdge(src, dst, e)
    g
  }

  override def addVertices[V, E](g: DDG[V, E])(v: Rs[V]): (Rs[V], DDG[V, E]) =
    ???

  override def removeVertex[V, E](g: DDG[V, E])(v: V): DDG[V, E] = {
    g.removeVertex(v)
    g
  }

  override def removeEdge[V, E](g: DDG[V, E])(src: V, dst: V, e: E): DDG[V, E] =
    ???

  override def get[V, E](g: DDG[V, E])(v: V): Option[V] = {
    if (g.containsVertex(v)) Option(v)
    else None
  }

  override def empty[V, E](implicit CT:ClassTag[E]): DDG[V, E] = {
    new DDG[V, E](CT.runtimeClass.asInstanceOf[Class[E]])
  }

}
