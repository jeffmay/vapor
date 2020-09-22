package com.rallyhealth.vapors.core.logic

trait Intersect[A] {

  def intersect(results: Seq[A]): A
}

object Intersect {

  def apply[A](implicit A: Intersect[A]): Intersect[A] = A

  implicit object boolean extends Intersect[Boolean] {
    override def intersect(results: Seq[Boolean]): Boolean = results.forall(identity)
  }

  implicit def set[A]: Intersect[Set[A]] = _.reduce(_ & _)

}
