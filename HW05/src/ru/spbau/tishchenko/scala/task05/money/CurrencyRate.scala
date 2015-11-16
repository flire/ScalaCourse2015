package ru.spbau.tishchenko.scala.task05.money

/**
 * Created by flire on 16.11.15.
 */
case class CurrencyRate(usd2rur: Double, eur2rur: Double) {
  def convertToRur(amount: Double, currency: String): Double = {
    currency match {
      case "USD" => amount * usd2rur
      case "EUR" => amount * eur2rur
    }
  }

  def convertFromRur(amount: Double, currency: String): Double = {
    currency match {
      case "USD" => amount / usd2rur
      case "EUR" => amount / eur2rur
    }
  }
}