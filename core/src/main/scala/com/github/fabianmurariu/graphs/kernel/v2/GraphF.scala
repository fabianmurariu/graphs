package com.github.fabianmurariu.graphs.kernel.v2

import cats.{Applicative, Eval, Id, Monad, Traverse}
import cats.kernel.Hash

trait GraphF[F[_], V, E] {

  def vertices: ResultSet[F, V]

  def addVertex(v: V): F[GraphF[F, V, E]]

  def addEdge(src: V, dst: V, e: E): F[GraphF[F, V, E]]

  def neighbours(v: V): ResultSet[F, V]

}

object GraphF {
  type Id[A] = A

  type MutableGraph[V, E] = GraphF[Id, V, E]

}

import cats.syntax.all.*

sealed abstract class ResultSet[F[_]: Monad, +O] {

  /** Give me a chunk of Os
    * @tparam O2
    * @return
    */
  def next[O2 >: O]: F[Vector[O2]]

  def map[O2](f: O => O2): ResultSet[F, O2] =
    next.map(os => ChunkRs(os.map(f)))

  def foldL[O2 >: O](o2: O2)(f: (O2, O) => F[O2]): F[O2] = {

    next.flatMap { os =>
      if (os.nonEmpty) {
        for {
          o2New <- os.foldLeftM(o2)(f)
          o2 <- foldL(o2New)(f)
        } yield o2
      } else Monad[F].pure(o2)
    }

  }
}

case class ChunkRs[F[_]: Monad, O](os: F[Vector[O]]) extends ResultSet[F, O] {

  /** Give me a chunk of Os
    *
    * @tparam O2
    * @return
    */
  override def next[O2 >: O]: F[Vector[O2]] = Monad[F].pure(os)
}
