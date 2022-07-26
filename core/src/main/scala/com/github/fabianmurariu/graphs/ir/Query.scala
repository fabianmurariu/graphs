package com.github.fabianmurariu.graphs.ir

sealed trait Query[+V, +E, +O <: Ref] {
  def merge(prev: (Ref, LogicalNode)*): Query[V, E, O]
  def exprs: Map[Ref, LogicalNode]
}

// NodeLookup(Person(..)) somewhat equivalent to (n:Person)
// it returns a frontier of node ids
// this sits at the base of the query tree
case class NodeLookup[V, E](v: Option[V]) extends Query[V, E, Node[V]] {

  override def exprs: Map[Ref, LogicalNode] = Map.empty

  def flatMap[O <: Ref](f: Node[V] => Query[V, E, O]): Query[V, E, O] = {
    val key = new Node[V]()
    val next = f(key)
    next.merge(key -> LogicalNode.NodeScan(v))
  }

  def merge(prev: (Ref, LogicalNode)*): Query[V, E, Node[V]] = this

}

case class Out[V, E](
  from: Node[V],
  e: Option[E],
  exprs: Map[Ref, LogicalNode] = Map.empty
) extends Query[V, E, Edge[E]] {

  def flatMap[O <: Ref](f: Edge[E] => Query[V, E, O]): Query[V, E, O] = {

    val key = new Edge[E]()
    val next = f(key)
    next.merge(key -> LogicalNode.ExpandOut(LogicalNode.LNRef(from), e))
  }

  def merge(prev: (Ref, LogicalNode)*): Query[V, E, Edge[E]] = {
    this.copy(exprs = exprs.++(prev))
  }
}
// case class Into[E](from: Node, e: Option[E]) extends Query[Nothing, E, Edge]

case class Dest[V, E](
  e: Edge[E],
  v: Option[V],
  exprs: Map[Ref, LogicalNode] = Map.empty
) extends Query[V, E, Node[V]] {

  override def merge(prev: (Ref, LogicalNode)*): Query[V, E, Node[V]] = {
    this.copy(exprs = exprs.++(prev))
  }

  def flatMap[O <: Ref](f: Node[V] => Query[V, E, O]): Query[V, E, O] = {
    val key = new Node[V]()
    val next = f(key)
    next.merge(key -> LogicalNode.NodeFilter(LogicalNode.LNRef(e), v))
  }
}

case class Return[V, E](
  rs: Vector[Ref],
  exprs: Map[Ref, LogicalNode] = Map.empty
) extends Query[V, E, Row] {
  override def merge(prev: (Ref, LogicalNode)*): Query[V, E, Row] = {
    this.copy(exprs = exprs.++(prev))
  }

  def map(f: Row => Row): Query[V, E, Row] = {
    val key = new Row
    val next = f(key)
    merge(next -> LogicalNode.Select(rs.map(LogicalNode.LNRef(_))))
  }
}

sealed trait Ref

class Node[A]() extends Ref

class Edge[A]() extends Ref

class Row() extends Ref

object Query {

  val q: Query[String, Int, Row] = for {
    src <- NodeLookup(Some("red"))
    e <- Out(src, Some(3))
    dst <- Dest(e, Some("green"))
    row <- Return(Vector(src, dst))
  } yield row
}

sealed trait LogicalNode

object LogicalNode {
  case class LNRef(r: Ref) extends LogicalNode
  case class NodeScan[A](v: Option[A]) extends LogicalNode
  case class ExpandOut[A](ln: LogicalNode, e: Option[A]) extends LogicalNode
  case class NodeFilter[A](ln: LogicalNode, v: Option[A]) extends LogicalNode
  case class Select(lns: Vector[LogicalNode]) extends LogicalNode
}
