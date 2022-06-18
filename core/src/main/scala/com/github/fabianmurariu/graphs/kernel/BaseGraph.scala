package com.github.fabianmurariu.graphs.kernel

/** Generic effectful directed graph 
  * no guarantees on multi edges
  * TODO: add traversals
  */
trait BaseGraph[F[_], V, E] {

  def out(v: V): Rs[F, (V, E)]
  def into(v: V): Rs[F, (V, E)]
  def neighbours(v: V): Rs[F, (V, E)] = out(v) ++ into(v)

  def outV(v: V): Rs[F, V]
  def intoV(v: V): Rs[F, V]
  def neighboursV(v: V): Rs[F, V] = outV(v) ++ intoV(v)
  def edges(src:V, dst:V):Rs[F, (V, E)]

  def isEmpty: F[Boolean]
  def contains(v: V): F[Option[V]]

  def vertices: Rs[F, V]
  def edges: Rs[F, (V, V, E)]

  def addEdge(src: V, dst: V, e: E): BaseGraph[F, V, E]
  def addVertex(v: V): BaseGraph[F, V, E]

  def removeVertex(v: V): BaseGraph[F, V, E]
  def removeEdge(src: V, dst: V): BaseGraph[F, V, E]
}


