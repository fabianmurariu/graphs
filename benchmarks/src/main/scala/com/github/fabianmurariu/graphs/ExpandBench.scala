package com.github.fabianmurariu.graphs

import com.github.fabianmurariu.graphs.ldbc.schema.{LdbcEdge, LdbcNode, Person}
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.TearDown

import java.time.LocalDateTime
import com.github.fabianmurariu.graphs.kernel.v2.DirectedGraphF
import com.github.fabianmurariu.graphs.ldbc.NodesLoader
import java.nio.file.Paths

@State(Scope.Thread)
class ExpandBench {

  val dynamic = Paths.get("/sf1/composite-merged-fk/initial_snapshot/dynamic")

  var graph: DirectedGraphF.LabeledDGraph64[LdbcNode, LdbcEdge] = _

  /*
   * Since @State objects are kept around during the lifetime of the benchmark,
   * it helps to have the methods which do state housekeeping. These are usual
   * fixture methods, you are probably familiar with them from JUnit and TestNG.
   *
   * Fixture methods make sense only on @State objects, and JMH will fail to compile
   * the test otherwise.
   *
   * As with the State, fixture methods are only called by those benchmark threads
   * which are using the state. That means, you can operate the thread-local contexts,
   * (don't) use synchronization as if you are executing in the context of benchmark
   * thread.
   *
   * Note: fixture methods can also work with static fields, although the semantics
   * of these operations fall back out of State scope, and obey usual Java rules (i.e.
   * one static field per class).
   */

  /*
   * Ok, let's prepare our benchmark:
   */

  @Setup
  def prepare: Unit = {
    graph = NodesLoader[LdbcNode].loadNodes[LdbcEdge](graph)(
      dynamic.resolve("Person")
    ) match {
      case Right(g) => g
      case Left(t)  => throw t
    }

  }

  /*
   * And, check the benchmark went fine afterwards:
   */

  @TearDown
  def check: Unit = {
    assert(graph.vertices.size > 0, "nothing changed?")
  }

  /*
   * This method obviously does the right thing, incrementing the field x
   * in the benchmark state. check() will never fail this way, because
   * we are always guaranteed to have at least one benchmark call.
   */

  @Benchmark
  def measureRight: Unit = {
    graph =
      graph.addVertex(1L, Person(LocalDateTime.now(), 1, "Blerg", "Blarg"))
  }

}
