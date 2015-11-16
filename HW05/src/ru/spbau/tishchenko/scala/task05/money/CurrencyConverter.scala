package ru.spbau.tishchenko.scala.task05.money

import java.text.SimpleDateFormat
import java.util.{Date, Calendar}
import scala.collection.mutable
import scala.collection.mutable.Map

import scala.xml.XML._
import scalaj.http.Http

/**
 * Created by flire on 16.11.15.
 */
object CurrencyConverter {
  val map: mutable.Map[String, CurrencyRate] = mutable.Map()

  def convertToRur(amount: Double, currency: String): Double =
    convertToRurAtDate(amount, currency, new SimpleDateFormat("dd/MM/YY").format(Calendar.getInstance().getTime))

  def convertToRurAtDate(amount: Double, currency: String, date: String): Double = {
      map.get(date) match {
        case Some(curRate) => curRate.convertToRur(amount, currency)
        case None => loadForDate(date).convertToRur(amount, currency)
      }
  }

  def convertFromRur(amount: Double, currency: String): Double =
    convertFromRurAtDate(amount, currency, new SimpleDateFormat("dd/MM/YY").format(Calendar.getInstance().getTime))

  def convertFromRurAtDate(amount: Double, currency: String, date: String): Double = {
    map.get(date) match {
      case Some(curRate) => curRate.convertFromRur(amount, currency)
      case None => loadForDate(date).convertFromRur(amount, currency)
    }
  }

  def loadForDate(date: String): CurrencyRate = {
    val response = loadString(
      Http("http://www.cbr.ru/scripts/XML_daily.asp").param("date_req", date).asString.body
    )
    val currencies = response \ "Valute"
    val usd = (currencies.filter(node => (node \ "CharCode").text.equals("USD") ).head \ "Value").text
    val eur = (currencies.filter(node => (node \ "CharCode").text.equals("EUR") ).head \ "Value").text
    val result = CurrencyRate(usd.toDouble, eur.toDouble)
    map += (date -> result)
    result
  }
}
