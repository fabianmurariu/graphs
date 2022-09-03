package graphs.jgrapht

import com.github.fabianmurariu.graphs.kernel.Graph
import org.jgrapht.graph.DefaultDirectedGraph

case class JGraphT[V, E](private[jgrapht] val graph: DefaultDirectedGraph[V, JEdge[E]])

object JGraphT {

  implicit val directedGraphInstance: Graph[JGraphT] =
    new JGraphTDirectedGraphInstance()

}
