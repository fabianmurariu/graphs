/*
 * Copyright 2022 32Bytes Software LTD
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.fabianmurariu.graphs.ir

// sealed trait Query[+V, +E, +O <: Ref] {
//   protected def merge(prev: (Ref, LogicalNode)*): Query[V, E, O]

//   protected def key: O

//   def exprs: Map[Ref, LogicalNode]
//   def map[A <: Ref](f: O => Vector[A]): Query[V, E, Ret] = {
//     val refs = f(key)
//     Query
//       .Project()
//       .merge(key -> LogicalNode.Select(refs.map { LogicalNode.LNRef(_) }))
//   }
// }

// object Query {

//   case class Project[V, E](exprs: Map[Ref, LogicalNode] = Map.empty)
//       extends Query[V, E, Ret] {

//     override protected def key: Ret = new Ret

//     override def merge(prev: (Ref, LogicalNode)*): Query[V, E, Ret] = {
//       this.copy(exprs = exprs.++(prev))
//     }

//   }

//   case class AddVertex[V, E](
//     vs: Vector[V],
//     exprs: Map[Ref, LogicalNode] = Map.empty,
//     key: Node[V] = new Node[V]
//   ) extends Query[V, E, Node[V]] {

//     def flatMap[O <: Ref](f: Node[V] => Query[V, E, O]): Query[V, E, O] = {
//       val next = f(key)
//       next.merge(key -> LogicalNode.AddNodes(vs, key))
//     }
//     override def merge(prev: (Ref, LogicalNode)*): Query[V, E, Node[V]] = {
//       this.copy(exprs = exprs.++(prev))
//     }
//   }
// // NodeLookup(Person(..)) somewhat equivalent to (n:Person)
// // it returns a frontier of node ids
// // this sits at the base of the query tree
//   case class NodeLookup[V, E](
//     v: Option[V],
//     exprs: Map[Ref, LogicalNode] = Map.empty,
//     key: Node[V] = new Node[V]()
//   ) extends Query[V, E, Node[V]] {

//     def flatMap[O <: Ref](f: Node[V] => Query[V, E, O]): Query[V, E, O] = {
//       val next = f(key)
//       next.merge(key -> LogicalNode.NodeScan(v))
//     }

//     def merge(prev: (Ref, LogicalNode)*): Query[V, E, Node[V]] = {
//       this.copy(exprs = exprs.++(prev))
//     }

//   }

//   case class Expand[V, E](
//     from: Node[V],
//     e: Option[E],
//     d: Direction,
//     exprs: Map[Ref, LogicalNode] = Map.empty,
//     key: Edge[E] = new Edge[E]()
//   ) extends Query[V, E, Edge[E]] {

//     def flatMap[O <: Ref](f: Edge[E] => Query[V, E, O]): Query[V, E, O] = {
//       val next = f(key)
//       val ln = d match {
//         case Direction.Out => LogicalNode.ExpandOut(LogicalNode.LNRef(from), e)
//         case Direction.In  => LogicalNode.ExpandIn(LogicalNode.LNRef(from), e)
//       }
//       next.merge(key -> ln)
//     }

//     def merge(prev: (Ref, LogicalNode)*): Query[V, E, Edge[E]] = {
//       this.copy(exprs = exprs.++(prev))
//     }
//   }

//   case class Dest[V, E](
//     e: Edge[E],
//     v: Option[V],
//     exprs: Map[Ref, LogicalNode] = Map.empty,
//     key: Node[V] = new Node[V]()
//   ) extends Query[V, E, Node[V]] {

//     override def merge(prev: (Ref, LogicalNode)*): Query[V, E, Node[V]] = {
//       this.copy(exprs = exprs.++(prev))
//     }

//     def flatMap[O <: Ref](f: Node[V] => Query[V, E, O]): Query[V, E, O] = {
//       val next = f(key)
//       next.merge(key -> LogicalNode.NodeFilter(LogicalNode.LNRef(e), v))
//     }
//   }

// }
sealed trait Ref

class Node[A] extends Ref

class Edge[A] extends Ref

case class Ret() extends Ref

class Row() extends Ref

sealed trait LogicalNode

object LogicalNode {
  // pointer to another node in the table
  case class LNRef(r: Ref) extends LogicalNode

  case class AddNodes[A](vs: Vector[A]) extends LogicalNode
  case class AddEdge[A](from: LogicalNode, dest: LogicalNode, es: A*)
      extends LogicalNode
  case class NodeScan[A](v: Option[A]) extends LogicalNode
  case class ExpandOut[A](ln: LogicalNode, e: Option[A]) extends LogicalNode
  case class ExpandIn[A](ln: LogicalNode, e: Option[A]) extends LogicalNode
  case class NodeFilter[A](ln: LogicalNode, v: Option[A]) extends LogicalNode
  case class Project(ln: LogicalNode.LNRef) extends LogicalNode
}

sealed trait Direction

object Direction {
  case object Out extends Direction
  case object In extends Direction
}
