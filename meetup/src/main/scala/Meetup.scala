import java.time.{DayOfWeek, LocalDate}

import Schedule.*
import java.time.temporal.TemporalAdjusters

case class Meetup(month: Int, year: Int):

  def day(dayOfWeek: Int, schedule: Schedule): LocalDate =
    val day          = DayOfWeek.of(dayOfWeek)
    val firstOfMonth = LocalDate.of(year, month, 1)

    schedule match
      case Last   => firstOfMonth.`with`(TemporalAdjusters.lastInMonth(day))
      case Teenth => firstOfMonth.withDayOfMonth(13).`with`(TemporalAdjusters.nextOrSame(day))
      case _ =>
        val ordinal = schedule.ordinal - Schedule.First.ordinal + 1
        firstOfMonth.`with`(TemporalAdjusters.dayOfWeekInMonth(ordinal, day))

enum Schedule:
  case First, Second, Third, Fourth, Last, Teenth

object Meetup:
  val Mon = DayOfWeek.MONDAY.getValue
  val Tue = DayOfWeek.TUESDAY.getValue
  val Wed = DayOfWeek.WEDNESDAY.getValue
  val Thu = DayOfWeek.THURSDAY.getValue
  val Fri = DayOfWeek.FRIDAY.getValue
  val Sat = DayOfWeek.SATURDAY.getValue
  val Sun = DayOfWeek.SUNDAY.getValue
