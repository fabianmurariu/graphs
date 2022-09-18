package com.github.fabianmurariu.graphs.kernel

import com.github.fabianmurariu.graphs.syntax._
import cats.kernel.Monoid

trait Program[S, V, E] extends Monoid[S] {

  def init[G[_, _]: Graph](g: G[V, E])(seed: Seed[V, S]): Rs[V]

  def gather[G[_, _]: Graph](g: G[V, E])(u: (V, S), e: (E, S), v: (V, S)): S

  def apply(v: (V, S), s: S): (V, S)

  def scatter(u: (V, S), e: (E, S), v: (V, S)): Scatter[V, S]

  def neighbours[G[_, _]: Graph](g: G[V, E])(v: V): Rs[(E, V)]

}

sealed trait Scatter[+V, +S]

object Scatter {

  case class Activate[V, S](v: V, es: S, dv: S) extends Scatter[V, S]

  case object Empty extends Scatter[Nothing, Nothing]
}

sealed trait Seed[A, S]

object Seed {
  case class Vertex[A, S](v: A, s: S) extends Seed[A, S]
  case class Initial[A, S](vs: Vector[(A, S)]) extends Seed[A, S]
}

class SSSP[V, E](implicit N: Numeric[E]) extends Program[Long, V, E] {

  override def combine(x: Long, y: Long): Long = Math.min(x, y)

  override def empty: Long = Long.MaxValue

  override def init[G[_, _]: Graph](g: G[V, E])(vs: Seed[V, Long]): Rs[V] =
    vs match {
      case Seed.Vertex(v, _) => g.out(Rs(v))
    }

  override def gather[G[_, _]: Graph](
    g: G[V, E]
  )(u: (V, Long), e: (E, Long), v: (V, Long)): Long = {
    val (_, dv) = v
    val (_, duv) = e
    dv + duv
  }

  override def apply(v: (V, Long), s: Long): (V, Long) = {
    val (vnext, _) = v
    (vnext, s)
  }

  override def scatter(
    u: (V, Long),
    edge: (E, Long),
    v: (V, Long)
  ): Scatter[V, Long] = {
    val (_, du) = u
    val (vertex, dv) = v
    val (e, _) = edge
    if (du + N.toLong(e) < dv)
      Scatter.Activate(vertex, du + N.toLong(e), N.toLong(e))
    else
      Scatter.Empty
  }

  override def neighbours[G[_, _]: Graph](g: G[V, E])(v: V): Rs[(E, V)] =
    g.outE(Rs(v))

}

object Program {

  case class ProgramCtx[S, V, E](
    vs: Map[V, S] = Map.empty[V, S],
    es: Map[(V, V, E), S] = Map.empty[(V, V, E), S]
  ) {

    def get(v: V): Option[S] = vs.get(v)

    def getOrElseV(v: V, orElse: => S): S =
      vs.getOrElse(v, orElse)

    def getOrElseE(v: V, u: V, e: E, orElse: => S): S =
      es.getOrElse((v, u, e), orElse)

    def updateVStateWith(v: V)(f: Option[S] => Option[S]): ProgramCtx[S, V, E] =
      this.copy(vs = vs.updatedWith(v)(f))

    def updateEStateWith(v: V, u: V, e: E)(
      f: Option[S] => Option[S]
    ): ProgramCtx[S, V, E] =
      this.copy(es = es.updatedWith((v, u, e))(f))
  }

  object ProgramCtx {
    def apply[S, V, E](seed: Seed[V, S]): ProgramCtx[S, V, E] = seed match {
      case Seed.Initial(vs)  => ProgramCtx(vs = Map(vs: _*))
      case Seed.Vertex(v, s) => ProgramCtx(vs = Map(v -> s))
    }
  }

  def runGather[S, V, E, G[_, _]:Graph](
    p: Program[S, V, E],
    u: V
  )(ctx: ProgramCtx[S, V, E], g: G[V, E]): Option[S] = {
    ctx.get(u).orElse {
      p
        .neighbours(g)(u)
        .map { case (e: E, v) =>
          p.gather(g)(
            v -> ctx.getOrElseV(u, p.empty),
            e -> ctx.getOrElseE(u, v, e, p.empty),
            u -> ctx.getOrElseV(v, p.empty)
          )
        }
        .reduceOption(p.combine)
    }
  }

  def runApply[S, V, E](uState: Option[S], u: V)(ctx: ProgramCtx[S, V, E]) = {
    uState.fold(ctx) { newAcc =>
      ctx.updateVStateWith(u) {
        case None       => Some(newAcc)
        case Some(acc0) => Some(p.combine(acc0, newAcc))
      }
    }
  }

  def run[S, V, E, G[_, _]](
    p: Program[S, V, E]
  )(g: G[V, E], seed: Seed[V, S])(implicit G: Graph[G]) = {

    val ctx = ProgramCtx[S, V, E](seed)

    p.init(g)(seed).foldLeft(ctx) { (ctx, u: V) =>
      // gather sum phase
      // if cached accumulator au for vertex u is empty then call gather
      val acc = runGather(p, u)(ctx, g)

      // apply phase
      val newCtx = runApply(acc, u)(ctx)

      // scatter phase
      p.neighbours(g)(u).toVector.foldLeft(newCtx) { case (ctx, (e, v)) =>
        p.scatter(
          v -> ctx.getOrElseV(u, p.empty),
          e -> ctx.getOrElseE(u, v, e, p.empty),
          u -> ctx.getOrElseV(v, p.empty)
        ) match {
          case Scatter.Activate(v, es, vs) =>
            ctx.updateVStateWith(v) {
              case Some(accV) => Some(p.combine(vs, accV))
              case None       => None
            }
          case Scatter.Empty =>
            ctx.updateVStateWith(v) { _ => None }
        }
        ctx
      }
    }

  }
}
