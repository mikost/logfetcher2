package name.mikkoostlund.logfetcher2

import java.io.IOException
import java.io.Closeable
import java.io.Reader
import java.io.File
import java.io.InputStream
import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.text.SimpleDateFormat
import java.util.Date
import java.nio.charset.Charset

object Main {
  val SDF = new SimpleDateFormat("MMM d H:m:s")

  def main(args: Array[String]) = {

    import TimeStampParsers.simpleDateFormat
    val sysLog = 
      new ScatteredLog("syslog", simpleDateFormat("MMM d H:m:s")) {
        val parts =
          GZIPLogPart("/var/log/syslog.7.gz") ::
          GZIPLogPart("/var/log/syslog.6.gz") ::
          GZIPLogPart("/var/log/syslog.5.gz") ::
          GZIPLogPart("/var/log/syslog.4.gz") ::
          GZIPLogPart("/var/log/syslog.3.gz") ::
          GZIPLogPart("/var/log/syslog.2.gz") ::
          NormalLogPart("/var/log/syslog.1") ::
          NormalLogPart("/var/log/syslog") ::
          Nil
      }

    val mysqlLog = 
      new ScatteredLog("kern", simpleDateFormat("MMM d H:m:s")) {
        val parts =
          GZIPLogPart("/var/log/kern.log.4.gz") ::
          GZIPLogPart("/var/log/kern.log.3.gz") ::
          GZIPLogPart("/var/log/kern.log.2.gz") ::
          NormalLogPart("/var/log/kern.log.1") ::
          NormalLogPart("/var/log/kern.log") ::
          Nil
      }

    val logs = sysLog :: mysqlLog :: Nil

    val startTime = SDF.parse("May 26 12:07:08")
    val startCriterion: Line => Boolean =
      line => line.time.isDefined && !line.time.get.before(startTime)

    val stopTime = SDF.parse("May 27 23:59:59")
    val stopCriterion: Line => Boolean = 
      line => line.time.isDefined && line.time.get.after(stopTime)

    import LogsReceivers.stdout
    LinesExtractor.run(logs, startCriterion, stopCriterion, stdout)
  }
}
