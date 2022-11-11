package com.github.fabianmurariu.graphs

import org.openjdk.jmh.annotations._

import java.util.LinkedList
import java.util.List

@State(Scope.Thread)
class JMHSample_26_BatchSize {

  /*
   * Sometimes you need to evaluate operation which doesn't have
   * the steady state. The cost of a benchmarked operation may
   * significantly vary from invocation to invocation.
   *
   * In this case, using the timed measurements is not a good idea,
   * and the only acceptable benchmark mode is a single shot. On the
   * other hand, the operation may be too small for reliable single
   * shot measurement.
   *
   * We can use "batch size" parameter to describe the number of
   * benchmark calls to do per one invocation without looping the method
   * manually and protect from problems described in JMHSample_11_Loops.
   */

  /*
   * Suppose we want to measure insertion in the middle of the list.
   */

  var list: List[String] = new LinkedList[String]

  /*
   * This is what you do with JMH.
   */
  @Benchmark
  @Warmup(iterations = 5, batchSize = 5000)
  @Measurement(iterations = 5, batchSize = 5000)
  @BenchmarkMode(Array(Mode.SingleShotTime))
  def measureRight: List[String] = {
    list.add(list.size / 2, "something")
    list
  }

  @Setup(Level.Iteration)
  def setup: Unit = list.clear

}
