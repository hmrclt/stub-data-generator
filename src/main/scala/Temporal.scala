package uk.gov.hmrc.smartstub

import org.scalacheck._
import Gen._
import java.time.LocalDate

trait Temporal extends Any {
  def date(start: LocalDate, end: LocalDate): Gen[LocalDate] = 
    choose(start.toEpochDay, end.toEpochDay).map(LocalDate.ofEpochDay)

  def date: Gen[LocalDate] = date (1970, 2000)

  def date(
    start: Int,
    end: Int
  ): Gen[LocalDate] = date(
    LocalDate.of(start, 1, 1),
    LocalDate.of(end, 12, 31)
  )
}
