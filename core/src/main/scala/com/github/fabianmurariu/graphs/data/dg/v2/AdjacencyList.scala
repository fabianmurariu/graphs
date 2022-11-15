package com.github.fabianmurariu.graphs.data.dg.v2

trait AdjacencyList[+E] {
  def vs: IndexedSeq[Int]
  def props: IndexedSeq[E]

  def appendPair[EE >: E](v: Int, e: EE): AdjacencyList[E]

  def foldLeft[B](b: B)(f: (B, Int) => B): B

  def remove(v: Int): AdjacencyList[E]

  def iterator: Iterable[(E, Int)] = props.view.zip(vs)
  def vertexIds: Iterable[Int] = vs.view
}
