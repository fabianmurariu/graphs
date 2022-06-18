package com.github.fabianmurariu.graphs.data

import cats.Id
import com.github.fabianmurariu.graphs.kernel.BaseGraph
import com.github.fabianmurariu.graphs.kernel.Rs

case class ImmutableHashGraph[V, E](vs: Map[V, Links[V, E]] = Map.empty)
    extends BaseGraph[Id, V, E] {

  def out(v: V): Rs[Id, (V, E)] =
    Rs.fromIter(vs.getOrElse(v, Links(v)).out)

  def into(v: V): Rs[Id, (V, E)] =
    Rs.fromIter(vs.getOrElse(v, Links(v)).into)

  def outV(v: V): Rs[Id, V] =
    Rs.fromIter(vs.getOrElse(v, Links(v)).out.keySet)

  def intoV(v: V): Rs[Id, V] =
    Rs.fromIter(vs.getOrElse(v, Links(v)).into.keySet)

  def isEmpty: Id[Boolean] = vs.isEmpty

  def contains(v: V): Id[Option[V]] = vs.get(v).map(_.v)

  def vertices: Rs[Id, V] =
    Rs.fromIter(vs.keySet)

  def edges: Rs[Id, (V, V, E)] = {
    val outEdges = vs.view.map { case (src, links) =>
      links.out.map { case (dst, e) => (src, dst, e) }
    }.flatten
    val inEdges = vs.view.map { case (dst, links) =>
      links.into.map { case (src, e) => (dst, src, e) }
    }.flatten

    Rs.fromIter(outEdges ++ inEdges)
  }
  def edges(src: V, dst: V): Rs[Id, (V, E)] = {
    val Links(_, out, _) = vs.getOrElse(src, Links(src))
    Rs.fromIter(out)
  }

  def addEdge(src: V, dst: V, e: E): ImmutableHashGraph[V, E] = {
    val g1 = vs
      .updatedWith(src) {
        case Some(links) => Some(links.addOutEdge(dst, e))
        case None        => Some(Links(src).addOutEdge(dst, e))
      }
      .updatedWith(dst) {
        case Some(links) => Some(links.addInEdge(src, e))
        case None        => Some(Links(dst).addInEdge(src, e))
      }

    ImmutableHashGraph(g1)
  }

  def addVertex(v: V): ImmutableHashGraph[V, E] = {
    val g1 = vs.updatedWith(v) {
      case None   => Some(Links(v))
      case exists => exists
    }
    ImmutableHashGraph(g1)
  }
  def removeVertex(v: V): ImmutableHashGraph[V, E] = {
    vs.get(v) match {
      case None => this
      case Some(links) =>
        val g1 = links.into.foldLeft(vs) { case (vs, (src, _)) =>
          vs.updatedWith(src) {
            case None        => None
            case Some(links) => Some(links.removeOut(v).removeInto(v))
          }
        }

        val g2 = links.out.foldLeft(g1) { case (vs, (dst, _)) =>
          vs.updatedWith(dst) {
            case None        => None
            case Some(links) => Some(links.removeOut(v).removeInto(v))
          }
        }
        ImmutableHashGraph(g2)
    }

  }

  def removeEdge(src: V, dst: V): ImmutableHashGraph[V, E] = {
    val g1 = vs
      .updatedWith(src) {
        case None           => None
        case Some(srcLinks) => Some(srcLinks.removeOut(dst))
      }
      .updatedWith(dst) {
        case None           => None
        case Some(dstLinks) => Some(dstLinks.removeInto(src))
      }

    ImmutableHashGraph(g1)
  }
}
