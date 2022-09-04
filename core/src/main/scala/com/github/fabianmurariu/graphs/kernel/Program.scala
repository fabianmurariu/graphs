package com.github.fabianmurariu.graphs.kernel

import com.github.fabianmurariu.graphs.syntax._
import cats.kernel.Monoid

trait Program[S, V, E] extends Monoid[S] {

  def gather[G[_, _]: Graph](g: G[V, E])(u: (V, S), e: (E, S), v: (V, S)): S

  def apply(v: (V, S), s: S): (V, S)

  def scatter(u: (V, S), e: (E, S), v: (V, S)): ((E, S), S)

  def neighbours[G[_, _]: Graph](g: G[V, E])(v: V): Rs[(V, E)]

}

object Program {


  case class ProgramCtx[S, V, E](vs: Map[V, S], es: Map[(V, V, E), S])

  def run[S, V, E, G[_, _]](
    p: Program[S, V, E]
  )(g: G[V, E])(implicit G: Graph[G]) = {

    G.vertices(g).foldLeft(Map.empty[V, S]) { (ctx, v: V) =>
      // gather sum phase
      val acc = p
        .neighbours(g)(v)
        .toVector
        .map { case (u, e: E) =>
          p.gather(g)(
            v -> ctx.getOrElse(v, p.empty),
            e -> p.empty,
            u -> ctx.getOrElse(u, p.empty)
          )
        }
        .reduceOption(p.combine)

      // apply phase
      val newCtx = acc.fold(ctx) { newAcc =>
        ctx.updatedWith(v) {
          case None       => Some(newAcc)
          case Some(acc0) => Some(p.combine(acc0, newAcc))
        }
      }

      // scatter phase
      p.neighbours(g)(v).toVector.foldLeft(newCtx) { case (ctx, (u, e)) =>
        val _ = p.scatter(
            v -> ctx.getOrElse(v, p.empty),
            e -> p.empty,
            u -> ctx.getOrElse(u, p.empty)
        )
        ctx
      }
    }

  }
}
