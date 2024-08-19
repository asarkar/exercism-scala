object Leap:
  def leapYear(year: Int): Boolean =
    val divisibleBy4 = year % 4 == 0
    val divisibleBy400 = year % 400 == 0
    val divisibleBy100 = year % 100 == 0

    divisibleBy400 || (divisibleBy4 && !divisibleBy100)
