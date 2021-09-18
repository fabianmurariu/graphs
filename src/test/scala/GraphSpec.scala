import org.scalacheck._
import Graph0Spec.Triplet
import Graph0Spec.Path

object Graph0Spec extends Properties("Graph0") {
  import Prop.{forAll, propBoolean}

  case class Triplet[V, E](src: V, e: E, dst: V)
  case class Path[V, E](ts: List[Triplet[V, E]])

  object Triplet {
    def gen[V: Arbitrary, E: Arbitrary]: Gen[Triplet[V, E]] =
      for {
        src <- Arbitrary.arbitrary[V]
        dst <- Arbitrary.arbitrary[V]
        e <- Arbitrary.arbitrary[E]
      } yield Triplet(src, e, dst)

    implicit def arbitrary[V: Arbitrary, E: Arbitrary]
        : Arbitrary[Triplet[V, E]] =
      Arbitrary(gen)
  }

  object Path {
    def gen[V: Arbitrary, E: Arbitrary]: Gen[Path[V, E]] =
      for {
        unique <- Gen.nonEmptyContainerOf[Set, V](Arbitrary.arbitrary[V])
        edge <- Arbitrary.arbitrary[E]
      } yield {
        val ts = unique.toList
          .sliding(2)
          .collect { case head :: tail :: _ =>
            Triplet(head, edge, tail)
          }
          .toList
        Path(ts)
      }

    implicit def arbitrary[V: Arbitrary, E: Arbitrary]: Arbitrary[Path[V, E]] =
      Arbitrary(gen)
  }

  property("iCanHazFriend") = iCanHazFriend[Int, String, DGraph]
  property("friendOfAFriend") = friendOfAFriend[Int, Boolean, DGraph]
  property("friendOfAFriend2") = friendOfAFriend2[Int, Boolean, DGraph]

  def iCanHazFriend[V: Arbitrary, E: Arbitrary, G[_, _]: Graph0] =
    forAll { (triplet: Triplet[V, E]) =>
      val g = Graph0[G].empty[V, E].addTriplets(Tuple.fromProductTyped(triplet))

      g.neighboursV(triplet.src)
        .find(_ == triplet.dst)
        .isDefined
    }

  def friendOfAFriend[V: Arbitrary, E: Arbitrary, G[_, _]: Graph0] =
    forAll { (path: Path[V, E]) =>
      val g = Graph0[G]
        .empty[V, E]
        .addTriplets(path.ts.map(Tuple.fromProductTyped(_)): _*)

      val actual = path.ts match
        case first :: _ => walk(g)(first.src).toVector
        case Nil        => Vector.empty

      val expected = g.vertices.toVector

      s"$actual != $expected" |: (actual == expected)
    }

  def walk[G[_, _]: Graph0, V, E](g: G[V, E])(start: V): LazyList[V] =
    g.neighbours(start).headOption match {
      case None => LazyList(start)
      case Some((node, _, next)) =>
        node #:: walk(g)(next)
    }

  def friendOfAFriend2[V: Arbitrary, E: Arbitrary, G[_, _]: Graph0] =
    forAll { (path: Path[V, E]) =>
      val g = Graph0[G]
        .empty[V, E]
        .addTriplets(path.ts.map(Tuple.fromProductTyped(_)): _*)

      val actual = path.ts.headOption
        .map { first =>
          Graph0[G].dfs[E].collect(g)(from = first.src, into = Vector)
        }
        .getOrElse(Vector.empty)

      val expected = g.vertices.toVector

      s"$actual != $expected" |: (actual == expected)
    }
}
