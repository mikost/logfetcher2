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

trait LogFetcherEngine {
  def run(logs: Traversable[Log], startCriterion: Line => Boolean, stopCriterion: Line => Boolean, logsReceiver: LogsReceiver) = {
    for {
      log      <- logs;
      stream   <- managed(logsReceiver.startReceiving(log.name));
      allLines <- managed(log.open);
      wantedLines0 = allLines.dropWhile { !startCriterion(_) }
      wantedLines = wantedLines0.takeWhile { !stopCriterion(_) };
      line     <- wantedLines } {
        stream.receiveLogLine(line.text)
    }
  }
}

object LogFetcherEngine extends LogFetcherEngine