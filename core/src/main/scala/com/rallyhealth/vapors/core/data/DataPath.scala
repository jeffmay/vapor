package com.rallyhealth.vapors.core.data

import cats.Show
import cats.data.NonEmptySet

import scala.annotation.tailrec
import scala.collection.{immutable, BitSet, SortedSet}

// TODO: Better data structure for appending?
final case class DataPath(nodes: List[DataPath.Node]) extends AnyVal {
  import DataPath._

  def apply(idx: Int): DataPath = atIndex(idx)

  def apply(
    startIdx: Int,
    endIdx: Int,
  ): DataPath = slice(startIdx, endIdx)

  def isEmpty: Boolean = nodes.isEmpty

  def indexes(at: NonEmptySet[Int]): DataPath = DataPath(nodes ::: IdxSet(BitSet.fromSpecific(at.toSortedSet)) :: Nil)

  def slice(
    startIdx: Int,
    endIdx: Int,
  ): DataPath = DataPath(nodes ::: IdxSlice(startIdx, endIdx) :: Nil)

  def between(idxRange: Range): DataPath = DataPath(nodes ::: IdxRange(idxRange) :: Nil)

  def atIndex(idx: Int): DataPath = DataPath(nodes ::: Idx(idx) :: Nil)

  def atHead: DataPath = DataPath(nodes ::: Head :: Nil)

  def atLast: DataPath = DataPath(nodes ::: Last :: Nil)

  def atKey[K : ValidDataPathKey](key: K): DataPath =
    DataPath(nodes ::: MapKey(ValidDataPathKey[K].stringify(key)) :: Nil)

  def filterKeys[K : ValidDataPathKey](keys: NonEmptySet[K]): DataPath = {
    import cats.instances.string._
    DataPath(nodes ::: FilterKeys(keys.map(ValidDataPathKey[K].stringify).toSortedSet) :: Nil)
  }

  def atField(name: String): DataPath = DataPath(nodes ::: Field(name) :: Nil)

  def :::(that: DataPath): DataPath = DataPath(that.nodes ::: this.nodes)
}

object DataPath {

  val empty: DataPath = DataPath(Nil)

  sealed trait Node

  // TODO: Make subclasses private for binary compatibility
  final case class Field(name: String) extends Node

  final case class MapKey(key: String) extends Node

  final case class FilterKeys(keys: SortedSet[String]) extends Node

  final case object Head extends Node

  final case object Last extends Node

  final case class Idx(index: Int) extends Node

  final case class IdxSlice(
    startIdx: Int,
    endIdx: Int,
  ) extends Node

  final case class IdxRange(range: Range) extends Node

  final case class IdxSet private (bitSet: BitSet) extends Node {

    /**
      * The [[IdxSet]] is designed to never be empty, so this is safe,
      * however, using [[BitSet]] is more performant.
      */
    def nonEmptySet: NonEmptySet[Int] = NonEmptySet.fromSetUnsafe(immutable.SortedSet.from(bitSet))
  }

  final object IdxSet {
    def apply(set: NonEmptySet[Int]): IdxSet = IdxSet(BitSet.fromSpecific(set.toSortedSet))

    def of(
      one: Int,
      others: Int*,
    ): IdxSet = IdxSet(BitSet(one) ++ others)
  }

  @tailrec private def writeToBuilder(
    buffer: StringBuilder,
    path: List[DataPath.Node],
  ): StringBuilder = path match {
    case Nil => buffer
    case head :: tail =>
      var remaining: List[DataPath.Node] = tail
      head match {
        case MapKey(key) =>
          buffer.append("['").append(key).append("']")
        case FilterKeys(keys) =>
          buffer.append("['")
          joinKeys(buffer, keys, "', '")
          buffer.append("']")
        case Field(name) =>
          buffer.append('.').append(name)
        case Head =>
          remaining ::= Idx(0)
        case Last =>
          remaining ::= Idx(-1)
        case Idx(idx) =>
          buffer.append('[').append(idx).append(']')
        case IdxSlice(startIdx, endIdx) =>
          buffer.append('[').append(startIdx).append(':').append(endIdx).append(']')
        case IdxRange(range) if range.step == 1 =>
          remaining ::= IdxSlice(range.start, range.end)
        case IdxRange(range) =>
          remaining ::= IdxSet(BitSet.fromSpecific(range))
        case IdxSet(idxSet) =>
          buffer.append('[')
          joinKeys(buffer, idxSet, ",")
          buffer.append(']')
      }
      writeToBuilder(buffer, remaining)
  }

  private def joinKeys[T](
    buffer: StringBuilder,
    items: IterableOnce[T],
    sep: String,
  ): Unit = {
    val iter = items.iterator
    if (iter.nonEmpty) {
      for (item <- iter) {
        buffer.append(item)
        buffer.append(sep)
      }
      buffer.setLength(buffer.length - sep.length)
    }
  }

  implicit val show: Show[DataPath] = Show.show { path =>
    DataPath.writeToBuilder(new StringBuilder(), path.nodes).result()
  }
}
