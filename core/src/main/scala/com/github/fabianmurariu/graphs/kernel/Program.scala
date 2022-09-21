package com.github.fabianmurariu.graphs.kernel

import cats.kernel.Monoid
import com.github.fabianmurariu.graphs.syntax.*

import scala.collection.mutable

trait Program[S, V, E] extends Monoid[S] {

  def init[G[_, _]: Graph](g: G[V, E])(seed: Seed[V, S]): Rs[V]

  def gather[G[_, _]: Graph](
    g: G[V, E]
  )(u: State[V, S], e: State[E, S], v: State[V, S]): S

  def apply(v: State[V, S], s: S): State[V, S]

  def scatter(u: State[V, S], e: State[E, S], v: State[V, S]): Scatter[V, S]

  def neighbours[G[_, _]: Graph](g: G[V, E])(v: V): Rs[(E, V)]

}

case class State[A, S](value: A, state: S, changed: Boolean) {
  def update(newState: S): State[A, S] = {
    this.copy(state = newState, changed = newState != state)
  }

  def get: S = state
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

  override def empty: Long = Math.pow(2, 60).toLong

  override def init[G[_, _]: Graph](g: G[V, E])(vs: Seed[V, Long]): Rs[V] = {
    println(vs)
    vs match {
      case Seed.Vertex(v, _) => g.out(Rs(v))
      case Seed.Initial(vs)  => g.out(Rs.fromIter(vs.map(_._1)))
    }
  }

  override def gather[G[_, _]: Graph](
    g: G[V, E]
  )(u: State[V, Long], e: State[E, Long], v: State[V, Long]): Long = {
    u.get + e.get
  }

  override def apply(v: State[V, Long], s: Long): State[V, Long] = {
    v.update(s)
  }

  override def scatter(
    u: State[V, Long],
    edge: State[E, Long],
    v: State[V, Long]
  ): Scatter[V, Long] = {
    val (_, du) = u
    val (vertex, dv) = v
    val (e, duv) = edge
    val newVal = du + N.toLong(e)
    if (newVal < dv)
      Scatter.Activate(vertex, newVal, N.toLong(e))
    else
      Scatter.Empty
  }

  override def neighbours[G[_, _]: Graph](g: G[V, E])(v: V): Rs[(E, V)] =
    g.outE(Rs(v)) ++ g.inE(Rs(v))

}

object Program {

  case class ProgramCtx[S, V, E](
    vs: Map[V, S] = Map.empty[V, S],
    es: Map[(V, V, E), S] = Map.empty[(V, V, E), S],
    active: Set[V]
  ) {

    def get(v: V): Option[S] = vs.get(v)

    def getOrElseV(v: V, orElse: => S): S =
      vs.getOrElse(v, orElse)

    def getOrElseE(v: V, u: V, e: E, orElse: => S): S =
      es.getOrElse((v, u, e), orElse)

    def updateVStateWith(v: V)(f: Option[S] => Option[S]): ProgramCtx[S, V, E] =
      this.copy(vs = vs.updatedWith(v)(f))

    def activate(v: V): ProgramCtx[S, V, E] = {
      this.copy(active = active + v)
    }

    def deactivate(v: V): ProgramCtx[S, V, E] = {
      this.copy(active = active - v)
    }
    def updateEStateWith(v: V, u: V, e: E)(
      f: Option[S] => Option[S]
    ): ProgramCtx[S, V, E] =
      this.copy(es = es.updatedWith((v, u, e))(f))
  }

  object ProgramCtx {
    def apply[S, V, E](seed: Seed[V, S]): ProgramCtx[S, V, E] = seed match {
      case Seed.Initial(vs) =>
        ProgramCtx(vs = Map(vs*), active = Set(vs.map(_._1)*))
      case Seed.Vertex(v, s) => ProgramCtx(vs = Map(v -> s), active = Set(v))
    }
  }

  def runGather[S, V, E, G[_, _]: Graph](
    p: Program[S, V, E],
    u: V
  )(ctx: ProgramCtx[S, V, E], g: G[V, E]): Option[S] = {
    // if cached accumulator for vertex u is empty then call gather
    ctx.get(u).orElse {
      p
        .neighbours(g)(u)
        .map { case (e, v) =>
          p.gather(g)(
            u -> ctx.getOrElseV(u, p.empty),
            e -> ctx.getOrElseE(u, v, e, p.empty),
            v -> ctx.getOrElseV(v, p.empty)
          )
        }
        .reduceOption(p.combine)
    }
  }

  def runApply[S, V, E](p: Program[S, V, E], uState: Option[S], u: V)(
    ctx: ProgramCtx[S, V, E]
  ): ProgramCtx[S, V, E] = {
    uState.fold(ctx) { newAcc =>
      ctx.updateVStateWith(u) {
        case None       => Some(newAcc)
        case Some(acc0) => Some(p.combine(acc0, newAcc))
      }
    }
  }

  def scatter[S, V, E, G[_, _]: Graph](p: Program[S, V, E], g: G[V, E], u: V)(
    initCtx: ProgramCtx[S, V, E]
  ): ProgramCtx[S, V, E] = {
    val expand = p.neighbours(g)(u)
    expand.foldLeft(initCtx) { case (ctx, (e, v)) =>
      p.scatter(
        u -> ctx.getOrElseV(u, p.empty),
        e -> ctx.getOrElseE(u, v, e, p.empty),
        v -> ctx.getOrElseV(v, p.empty)
      ) match {
        case Scatter.Activate(v, es, vs) =>
          val nextCtx = ctx
            .updateVStateWith(v) {
              case Some(accV) => Some(p.combine(vs, accV))
              case None       => Some(vs)
            }
            .updateEStateWith(v, u, e) {
              case Some(accE) => Some(p.combine(es, accE))
              case None       => Some(es)
            }
            .activate(v)
          nextCtx
        case Scatter.Empty =>
          ctx.deactivate(v) // .updateVStateWith(v) { _ => None }
      }
    }
  }

  def run[S, V, E, G[_, _]](
    p: Program[S, V, E]
  )(g: G[V, E], seed: Seed[V, S])(implicit G: Graph[G]): ProgramCtx[S, V, E] = {

    var ctx = ProgramCtx[S, V, E](seed)

    while (ctx.active.nonEmpty) {
      ctx = ctx.active.foldLeft(ctx) { (ctx, u: V) =>
        // gather sum phase
        val acc = runGather(p, u)(ctx, g)

        // apply phase
        val newCtx = runApply(p, acc, u)(ctx)

        // scatter phase
        scatter(p, g, u)(newCtx).deactivate(u)
      }
    }
    ctx

  }
}
