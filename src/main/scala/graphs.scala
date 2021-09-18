import scala.collection.Factory
import scala.annotation.tailrec
import scala.collection.immutable.Queue

type DGraph[V, E] = Map[V, (V, Map[V, E])]

trait Graph0[G[_, _]]:
  extension [V, E](g: G[V, E])
    def neighbours(v: V): Iterable[(V, E, V)]

    def vertices: Iterable[V]
    def triplets: Iterable[(V, E, V)]

    def addV(v: V): G[V, E]
    def addEdge(src: V, e: E, dst: V): G[V, E]
    def addTriplets(ts: (V, E, V)*): G[V, E]

    def neighboursV(v: V): Iterable[V] =
      neighbours(v).map { case (_, _, dst) => dst }

  def empty[V, E]: G[V, E]

  def dfs[E] = traverse[E, List]

  def traverse[E, F[_]: TraversalSupport] =
    new GraphTraversal[[A] =>> G[A, E]] {
      extension [A](g: G[A, E])
        def traversal[B](start: A, b: B)(acc: (B, A) => B): B =
          @tailrec
          def loop(stack: F[A], seen: Set[A], b: B): B =
            stack.take match {
              case None => b
              case Some((first, rest)) => // in other words stack.pop
                val newB = acc(b, first)
                val newStack = g
                  .neighboursV(first)
                  .filterNot(seen)
                  .foldLeft(rest) { (s, child) => s.offer(child) }
                loop(newStack, seen + first, newB)
            }

          loop(TraversalSupport[F].from(start), Set(start), b)
    }

object Graph0:
  def apply[G[_, _]](using G: Graph0[G]) = G

trait GraphTraversal[F[_]]:
  extension [A](f: F[A])
    def traversal[B](start: A, b: B)(acc: (B, A) => B): B

    def collect[C1](from: A, into: Factory[A, C1]): C1 =
      traversal(from, into.newBuilder)((b, a) => b += a).result

trait TraversalSupport[F[_]]:
  extension [A](f: F[A])
    def take: Option[(A, F[A])]
    def offer(a: A): F[A]
  def from[A](a: A): F[A]

object TraversalSupport:
  def apply[F[_]](using TS: TraversalSupport[F]) = TS

  given TraversalSupport[List] with
    extension [A](l: List[A])
      def take = l match {
        case Nil          => None
        case head :: tail => Some(head -> tail)
      }

      def offer(a: A) = a :: l
    def from[A](a: A) = List(a)

  given TraversalSupport[Queue] with
    extension [A](q: Queue[A])
      def take = q.dequeueOption
      def offer(a: A) = q.enqueue(a)
    def from[A](a: A) = Queue(a)

given Graph0[DGraph] with
  extension [V, E](g: DGraph[V, E])

    def neighbours(v: V): Iterable[(V, E, V)] =
      g.get(v)
        .toSeq
        .flatMap { case (src, dsts) =>
          dsts.map { case (dst, e) => (src, e, dst) }
        }

    def vertices: Iterable[V] = g.keys

    def triplets: Iterable[(V, E, V)] = Iterable.empty

    def addV(v: V): DGraph[V, E] =
      g.get(v) match {
        case None    => g + (v -> (v, Map.empty))
        case Some(_) => g
      }

    def addEdge(src: V, e: E, dst: V): DGraph[V, E] =
      val graphOpt = for {
        s <- g.get(src)
        d <- g.get(dst)
        (srcV: V, edges: Map[V, E]) = s
        (dstV: V, _) = d
      } yield (g.updated(srcV, srcV -> (edges + (dstV -> e))))
      graphOpt.getOrElse(g)

    def addTriplets(ts: (V, E, V)*): DGraph[V, E] =
      ts.foldLeft(g) { case (g, (src, edge, dst)) =>
        g.addV(src).addV(dst).addEdge(src, edge, dst)
      }

  def empty[V, E]: DGraph[V, E] = Map.empty

def iCanHazFriend[G[_, _]: Graph0, V, E](triplet: (V, E, V)): Boolean =
  val g = Graph0[G].empty[V, E].addTriplets(triplet)
  triplet match {
    case (src, edge, dst) =>
      g.neighbours(src).find(_ == dst).isDefined
  }

trait graphs[IO[_]]:
  trait Graph1[G[_, _]]:
    extension [V, E](g: G[V, E]) def neighbours(v: V): IO[Iterable[(E, V)]]
