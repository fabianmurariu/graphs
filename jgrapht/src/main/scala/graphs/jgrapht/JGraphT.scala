package graphs.jgrapht

import com.github.fabianmurariu.graphs.kernel.Graph
import org.jgrapht.graph.DefaultDirectedGraph

object JGraphT {

  implicit val directedGraphInstance: Graph[DefaultDirectedGraph] =
    new JGraphTDirectedGraphInstance()

}
