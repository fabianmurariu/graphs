package com.github.fabianmurariu.graphs.kernel

import cats.Id

trait Graph[F[_], G[_, _]]:
  extension [V, E](g: G[V, E])
    def neighbours(v: V): ResultSet[F, (V, E, V)]
    def addVertex(v: V): F[G[V, E]]
    def addEdge(src: V, e: E, dst: V): F[G[V, E]]
    def vertices: ResultSet[F, V]
    def edge(src: V, dst: V): ResultSet[F, (V, E, V)]

  def empty[V, E]: F[G[V, E]]

object Graph:
  def apply[F[_], G[_, _]](using g: Graph[F, G]): Graph[F, G] = g


trait Support[F[_]]

object Support:
    given nopeId:Support[Id] = new Support[Id]{}