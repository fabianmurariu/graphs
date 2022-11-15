package com.github.fabianmurariu.graphs.kernel

import cats.Monad
import alleycats.std.iterable.*
import cats.syntax.all.*
import scala.collection.Factory

sealed trait ResultSet[F[_], +O] extends Any with Serializable {

  def size(implicit F: Monad[F]): F[Long]

  /** Give me a chunk of Os
    * @tparam O2
    * @return
    */
  def next[O2 >: O](implicit F: Monad[F]): F[Iterable[O2]]

  def to[COL](CC:Factory[O, COL])(implicit F: Monad[F]): F[COL] = 
    next.map(_.to(CC))

  def map[O2](f: O => O2): ResultSet[F, O2] =
    ResultSet.MapRs(this, f)

  def foldLeft[O2 >: O](
    o2: O2
  )(f: (O2, O) => O2)(implicit F: Monad[F]): F[O2] = {
    next.flatMap { os =>
      if (os.nonEmpty) {
        val o2New = os.foldLeft(o2)(f)
        foldLeft(o2New)(f)
      } else F.pure(o2)
    }
  }

  def foldL[O2 >: O](
    o2: O2
  )(f: (O2, O) => F[O2])(implicit F: Monad[F]): F[O2] = {

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

object ResultSet {
  case class MapRs[F[_], O, O3](rs: ResultSet[F, O], f: O => O3)
      extends ResultSet[F, O3] {

    override def size(implicit F: Monad[F]): F[Long] = rs.size

    def next[O2 >: O3](implicit F: Monad[F]): F[Iterable[O2]] =
      rs.next.map(_.map(f))
  }

  case class EmptyRs[F[_], O]() extends ResultSet[F, O] {

    override def size(implicit F: Monad[F]): F[Long] = F.pure(0L)

    override def next[O2 >: O](implicit F: Monad[F]): F[Iterable[O2]] =
      F.pure(Vector.empty)

  }

  case class PureRs[F[_], O](os: F[Iterable[O]]) extends ResultSet[F, O] {

    override def size(implicit F: Monad[F]): F[Long] = os.map(_.size.toLong)

    override def next[O2 >: O](implicit F: Monad[F]): F[Iterable[O2]] =
      os.asInstanceOf[F[Iterable[O2]]]

  }

  /** Optimisation to attempt lifting the ids without calling the lookup
    *
    * @param ids
    * @param f
    */
  case class Ids[F[_], O](
    ids: F[Iterable[Int]],
    f: Iterable[Int] => F[Iterable[O]]
  ) extends ResultSet[F, O] {

    override def size(implicit F: Monad[F]): F[Long] = ids.map(_.size.toLong)

    override def next[O2 >: O](implicit F: Monad[F]): F[Iterable[O2]] =
      ids.flatMap { f }.asInstanceOf[F[Iterable[O2]]]

  }

  case class One[F[_], O](o: O) extends AnyVal with ResultSet[F, O] {

    override def size(implicit F: Monad[F]): F[Long] = F.pure(1)

    override def next[O2 >: O](implicit F: Monad[F]): F[Iterable[O2]] =
      F.pure(Vector(o))

  }

}
