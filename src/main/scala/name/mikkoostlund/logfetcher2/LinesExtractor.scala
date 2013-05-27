package name.mikkoostlund.logfetcher2

import java.util.Date
import java.io.Closeable

import resource.managed

trait Line {
  def text: String
  def time: Option[Date]
}

trait Lines extends Iterator[Line] with Closeable

trait Log {
  def name: String
  def open: Lines
}

trait LogsReceiver {
  trait ReceiverStream extends Closeable {
    def receiveLogLine(logText: String)
    def close = stopReceiving
  }

  def startReceiving(logName: String): ReceiverStream
  protected def stopReceiving()
}

trait LinesExtractor {
  def run(logs: Traversable[Log], startCriterion: Line => Boolean, stopCriterion: Line => Boolean, logsReceiver: LogsReceiver) = {

    validateParams(logs, startCriterion, stopCriterion, logsReceiver)

    for {
      log      <- logs;
      stream   <- managed(logsReceiver.startReceiving(log.name));
      allLines <- managed(log.open);
      wantedLines = allLines.dropWhile { !startCriterion(_) }.
                             takeWhile { !stopCriterion(_) };
      line     <- wantedLines } {
        stream.receiveLogLine(line.text)
    }
  }

  private[this] def validateParams(logs: Traversable[Log], startCriterion: Line => Boolean, stopCriterion: Line => Boolean, logsReceiver: LogsReceiver) {
    if (logs == null) throw new NullPointerException("logs")
    if (startCriterion == null) throw new NullPointerException("startCriterion")
    if (stopCriterion == null) throw new NullPointerException("stopCriterion")
    if (logsReceiver == null) throw new NullPointerException("logsReceiver")
  }
}

object LinesExtractor extends LinesExtractor
