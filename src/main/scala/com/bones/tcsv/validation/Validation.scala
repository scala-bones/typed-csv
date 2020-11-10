package com.bones.tcsv.validation

trait Validation[A] {
  /** Returns true if T passes the validation */
  def isValid: A => Boolean

  /** If t is not valid, this will return the error message. */
  def defaultError(t: A): String

  /** Gives an English text description of the validation. */
  def description: String
}


case class MaxLength(max: Int) extends Validation[String] {
  val isValid: String => Boolean = _.length <= max

  override def defaultError(t: String): String = s"$t is greater than $max"

  override def description: String = s"maximum of $max"
}

case class Max(maxLong: Int) extends Validation[Int] {
  override def isValid: Int => Boolean = _ <= maxLong

  override def defaultError(t: Int): String =
    s"$t is greater than $maxLong"

  override def description: String = s"maximum of $maxLong"
}

case class Min(minLong: Int) extends Validation[Int] {
  override def isValid: Int => Boolean = _ >= minLong

  override def defaultError(t: Int): String = s"$t is less than $minLong"

  override def description: String = s"minimum of $minLong"
}
