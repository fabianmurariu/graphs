package com.github.fabianmurariu.graphs.data.dg

trait AdjacencyList[+E] {
  def vs: IndexedSeq[Int]
  def props: IndexedSeq[E]

  def appendPair[EE >: E](v: Int, e: EE): AdjacencyList[E]

  def foldLeft[B](b: B)(f: (B, Int) => B): B

  def remove(v: Int): AdjacencyList[E]

  def iterator: Iterable[(E, Int)] = props.view.zip(vs)
  def vertexIds: Iterable[Int] = vs.view
}

case class VecStore[E](
  vs: Vector[Int] = Vector.empty,
  props: Vector[E] = Vector.empty
) extends AdjacencyList[E] {

  override def remove(v: Int): AdjacencyList[E] = {
    val newVs = vs.filter(_ != v)
    val newProps = props.view.zipWithIndex.collect {
      case (e, vi) if vs(vi) != v => e // remove all edges linked to v
    }.toVector
    VecStore(newVs, newProps)
  }

  override def foldLeft[B](b: B)(f: (B, Int) => B): B =
    vs.foldLeft(b)(f)

  override def appendPair[EE >: E](v: Int, e: EE): AdjacencyList[E] =
    this.copy(vs = vs :+ v, props :+ e.asInstanceOf[E]) // FIXME: possible bug

}
