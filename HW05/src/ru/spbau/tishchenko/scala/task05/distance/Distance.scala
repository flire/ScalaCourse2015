package ru.spbau.tishchenko.scala.task05.distance

/**
 * Created by flire on 15.11.15.
 */
class DistanceConversion(val value: Double) {
  def km = Km(value)
  def m = M(value)
  def mi = Mi(value)
  def ft = Ft(value)
  def yd = Yd(value)
  def in = In(value)
}

trait DistanceConversions {
  def fromSI(amount: Double): Distance
}

case object km extends DistanceConversions {
  override def fromSI(amount: Double) = Km(amount / Km.IN_METERS)
}

case object m extends DistanceConversions {
  override def fromSI(amount: Double) = M(amount / M.IN_METERS)
}

case object mi extends DistanceConversions {
  override def fromSI(amount: Double) = Mi(amount / Mi.IN_METERS)
}

case object ft extends DistanceConversions {
  override def fromSI(amount: Double) = Ft(amount / Ft.IN_METERS)
}

case object yd extends DistanceConversions {
  override def fromSI(amount: Double) = Yd(amount / Yd.IN_METERS)
}

case object in extends DistanceConversions {
  override def fromSI(amount: Double) = In(amount / In.IN_METERS)
}

object Distance {
  implicit def double2valconversion(value: Double): DistanceConversion = new DistanceConversion(value)
}

trait Distance {
  val amount: Double
  protected val IN_SI: Double
  def to(unit: DistanceConversions): Distance =
    unit.fromSI(toSI())
  protected def toSI(): Double = amount * IN_SI
}

object Km {
  val IN_METERS = 1000.0
  def fromSI(amount: Double) = Km(amount / IN_METERS)
}

case class Km(amount: Double) extends Distance {
  override protected val IN_SI = Km.IN_METERS
}

object M {
  val IN_METERS = 1.0
  def fromSI(amount: Double) = M(amount / IN_METERS)
}

case class M(amount: Double) extends Distance {
  override protected val IN_SI = M.IN_METERS
}

object Mi {
  val IN_METERS = 1609.34
}

case class Mi(amount: Double) extends Distance {
  override protected val IN_SI = Mi.IN_METERS
}

object Ft {
  val IN_METERS = 0.3048
}

case class Ft(amount: Double) extends Distance {
  override protected val IN_SI = Ft.IN_METERS
}

object Yd {
  val IN_METERS = 0.914
}

case class Yd(amount: Double) extends Distance {
  override protected val IN_SI = Yd.IN_METERS
}

object In {
  val IN_METERS = 0.0254
}

case class In(amount: Double) extends Distance {
  override protected val IN_SI = In.IN_METERS
}



