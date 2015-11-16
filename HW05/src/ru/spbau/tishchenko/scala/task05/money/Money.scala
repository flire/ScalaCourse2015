/**
 * Created by flire on 15.11.15.
 */
package ru.spbau.tishchenko.scala.task05.money

import ru.spbau.tishchenko.scala.task05.money._

/**
 * Created by flire on 15.11.15.
 */
class MoneyConversion(val value: Double) {
  def rur = Roubles(value)
  def usd = USDollars(value)
  def eur = Euros(value)
}

trait MoneyConversions {
  def fromRoubles(amount: Double): Money
}

object MoneyConversions {
  implicit def double2valconversion(value: Double): MoneyConversion = new MoneyConversion(value)
}

case object rur extends MoneyConversions {
  override def fromRoubles(amount: Double) = Roubles(amount)
}

case object usd extends MoneyConversions {
  override def fromRoubles(amount: Double) = USDollars(CurrencyConverter.convertFromRur(amount, "USD"))
}

case object eur extends MoneyConversions {
  override def fromRoubles(amount: Double) = Euros(CurrencyConverter.convertFromRur(amount, "EUR"))
}

trait Money {
  val amount: Double
  protected val code: String
  def to(currency: MoneyConversions): Money =
    currency.fromRoubles(toRoubles())
  protected def toRoubles(): Double = CurrencyConverter.convertToRur(amount, code)
}

case class Roubles(val amount: Double) extends Money {
  override protected val code = "RUR"
}

case class Euros(val amount: Double) extends Money {
  override protected val code = "EUR"
}

case class USDollars(val amount: Double) extends Money {
  override protected val code = "USD"
}





