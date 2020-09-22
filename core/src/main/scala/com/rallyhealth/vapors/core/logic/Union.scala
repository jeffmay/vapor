package com.rallyhealth.vapors.core.logic

trait Union[A] {

  def union(results: Seq[A]): A
}

object Union {

  def apply[A](implicit A: Union[A]): Union[A] = A

  implicit object boolean extends Union[Boolean] {
    override def union(results: Seq[Boolean]): Boolean = results.exists(identity)
  }

  implicit def set[A]: Intersect[Set[A]] = _.reduce(_ | _)
}
