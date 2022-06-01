package com.github.fabianmurariu.graphs.kernel

import org.scalacheck.{Gen, Arbitrary}
import cats.MonadError
import munit.CatsEffectSuite
import org.scalacheck.effect.PropF
import cats.syntax.all._

abstract class GraphSuite[G[_, _], F[_]: Support, V: Arbitrary](using
    G: Graph[F, G],
    F: MonadError[F, Throwable]
) extends CatsEffectSuite
    with munit.ScalaCheckEffectSuite {
  test("it can insert all vertices and get them back") {
    PropF.forAllF { (vs: Set[V]) =>
      val g = Graph[F, G].empty[V, String]

      for {
        g <- Graph[F, G].empty[V, String]
        g <- vs.toVector.foldLeftM(g) { (g, v) => g.addVertex(v) }
        allContains <- vs.toVector
          .map { v => g.contains(v) }
          .reduceOption((a1, a2) => a1.map2(a2)(_ & _))
          .getOrElse(F.pure(true))
        vertices <- g.vertices.toVector
      } yield {
        assert(allContains, "some vertices missing!")
        assertEquals(vertices, vs.toVector)
      }

    }

  }

  test("it can create a path from a list of nodes") {
    PropF.forAllF { (vs: NonEmptySet[Int]) =>
      linkPath(vs.s).flatMap { g =>
        g.edges.to(List).map { edges =>
          assert(edges.nonEmpty)
        }
      }
    }
  }

  test("a vector removed is not pointed at by anyone") {
    PropF.forAllF { (vs: NonEmptySet[Int]) =>
      for {
        g <- linkPath(vs.s) >>= (_.removeVertex(vs.head))
        edges <- g.edges.toSet
        remainingVs = edges.flatMap { case (v1, _, v2) => Set(v1, v2) }
        vertices <- g.vertices.toSet
      } yield {
        assertEquals(vertices, vs - vs.head)
        assert(!remainingVs(vs.head))
      }
    }
  }

  def linkPath[V](vs: Set[V]): F[G[V, String]] = {

    val vec = vs.toVector
    val pairs = vec.zip(vec.tail :+ vec.head)

    for {
      g <- Graph[F, G].empty[V, String]
      g <- pairs.foldLeftM(g) { case (g, (src, dst)) =>
        g.addVertex(src) >>= (_.addVertex(dst)) >>= (_.addEdge(
          src,
          s"${src}_${dst}",
          dst
        ))
      }
      _ <- pairs.map { case (src, dst) =>
        for {
          out <- g.out(src).to(List)
          into <- g.into(dst).to(List)
        } yield {
          assertEquals(out, List(dst))
          assertEquals(into, List(src))
        }
      }.sequence_
    } yield g

  }
}

case class NonEmptySet[V](s: Set[V]) {
  def head = s.head
  def -(v: V) = s - v
}

object NonEmptySet {
  given nesArbitrary[V: Arbitrary]: Arbitrary[NonEmptySet[V]] = {
    val genV = summon[Arbitrary[V]].arbitrary
    Arbitrary(Gen.nonEmptyContainerOf[Set, V](genV).map(NonEmptySet(_)))
  }
}
