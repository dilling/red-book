package chapter04
import chapter04.Option._

def toIntOption(s: String): Option[Int] =
  try Some(s.toInt)
  catch case _: NumberFormatException => None

/**
 * Top secret formula for computing an annual car
 * insurance premium from two key factors.
 */
def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 
  age * numberOfSpeedingTickets

def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Option[Double] =
  val optAge: Option[Int] = toIntOption(age)
  val optTickets: Option[Int] = toIntOption(numberOfSpeedingTickets)

  map2(optAge, optTickets)(insuranceRateQuote)
