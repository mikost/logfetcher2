package name.mikkoostlund.logfetcher2

import java.text.SimpleDateFormat

object TimeStampParsers {
  def simpleDateFormat(pattern: String): TimeStampParser =
    new TimeStampParser {
      lazy val SDF = new SimpleDateFormat(pattern)
      override def parse(line: String) = Option(SDF.parse(line))
    }
}

import java.util.Date

trait TimeStampParser {
  def parse(line: String): Option[Date]
}
