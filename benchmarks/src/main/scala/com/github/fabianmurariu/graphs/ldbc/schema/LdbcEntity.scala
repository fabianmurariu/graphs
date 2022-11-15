package com.github.fabianmurariu.graphs.ldbc.schema

import com.github.fabianmurariu.graphs.ldbc.{EdgeDecoder, NodeDecoder}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.control.NonFatal

sealed trait LdbcEntity

sealed trait LdbcNode extends LdbcEntity
sealed trait LdbcEdge extends LdbcEntity

object LdbcNode {

  implicit def nodeDecoder: NodeDecoder[LdbcNode] = new NodeDecoder[LdbcNode] {

    override def decodeLine(line: String): Either[Throwable,LdbcNode] = ???

    override def id(v: LdbcNode): Long = ???

  }
}

case class Person(
  created: LocalDateTime,
  id: Long,
  firstName: String,
  lastName: String
) extends LdbcNode 

object Person {
  implicit val decoder: NodeDecoder[Person] = new NodeDecoder[Person] {

    def id(p: Person): Long = p.id
    override def decodeLine(line: String): Either[Throwable, Person] = {
      try {
        val tokens = line.split("\\|")
        val createdDate =
          LocalDateTime.parse(tokens(0), DateTimeFormatter.ISO_ZONED_DATE_TIME)
        val id = tokens(1).toLong
        val firstName = tokens(2)
        val lastName = tokens(3)
        Right(Person(createdDate, id, firstName, lastName))
      } catch {
        case NonFatal(t) =>
          Left(new IllegalArgumentException(s"Failed to parse ${line}", t))
      }
    }
  }
}

case class PersonToPerson(
  created: LocalDateTime,
  personId1: Long,
  personId2: Long
) extends LdbcEdge

object PersonToPerson {
  implicit val decoder: EdgeDecoder[PersonToPerson] =
    new EdgeDecoder[PersonToPerson] {

      def dst(e: PersonToPerson): Long = e.personId2
      def src(e: PersonToPerson): Long = e.personId1

      override def decodeLine(
        line: String
      ): Either[Throwable, PersonToPerson] = {
        try {
          val tokens = line.split("\\|")
          val createdDate =
            LocalDateTime.parse(
              tokens(0),
              DateTimeFormatter.ISO_ZONED_DATE_TIME
            )
          val p1 = tokens(1).toLong
          val p2 = tokens(2).toLong
          Right(PersonToPerson(createdDate, p1, p2))
        } catch {
          case NonFatal(t) =>
            Left(new IllegalArgumentException(s"Failed to parse ${line}", t))
        }

      }
    }
}
