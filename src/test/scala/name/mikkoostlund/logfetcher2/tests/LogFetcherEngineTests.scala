package name.mikkoostlund.logfetcher2.tests

import java.util.Date
import java.text.SimpleDateFormat

import org.scalatest.FunSpec
import org.scalamock.scalatest.proxy.MockFactory

import name.mikkoostlund.logfetcher2.LogFetcherEngine
import name.mikkoostlund.logfetcher2.Log
import name.mikkoostlund.logfetcher2.Line
import name.mikkoostlund.logfetcher2.Lines
import name.mikkoostlund.logfetcher2.LogsReceiver

abstract class LogStub(val name: String) extends Log {

  var openedLinesObjs: List[LinesStub] = Nil

  override def open: Lines = {
    val newLines = new LinesStub(Iterator(lines: _*))
    openedLinesObjs = newLines :: openedLinesObjs
    newLines
  }

  lazy val lines: Seq[Line] = lineTimes map { lineTime: Int =>
    new LineStub(timeStampFrom(lineTime), lineTextFor(lineTime))
  }

  private[this]
  def timeStampFrom(lineTime: Int): Date =
    if (lineTime < 0) null else new Date(lineTime)

  private[this]
  def lineTextFor(lineTime: Int) = {
    val lineIndex0 = lineIndex
    lineIndex += 1
    "time "+ lineTime +": line "+ lineIndex0
  }

  private[this]
  var lineIndex = 0

  protected[this] def lineTimes: Seq[Int]
}

class LinesStub(private[this] val lines: Iterator[Line]) extends Lines {
  var closed = false
  def close(): Unit = {closed = true}
  def hasNext: Boolean = { checkOpen; lines.hasNext }
  def next(): Line =     { checkOpen; lines.next }

  private[this] def checkOpen = if (closed) throw new IllegalStateException
}

class LineStub(private[this] val __time: Date, val text: String) extends Line {
  val time = Option(__time)
}

object LineStub {
   val NO_TIME = -1
}

class LogFetcherEngineTest extends FunSpec with MockFactory {

  trait Log1 {
    val log1 = new LogStub("log1") {
      val lineTimes = Seq(0,1,2,3,LineStub.NO_TIME,5,6,7,8,9)
    }
  }

  def setupExpectations(logsReceiver: LogsReceiver with org.scalamock.proxy.Mock, log: LogStub, startLine: Int, stopLine: Int): Unit = {
    val stream = mock[logsReceiver.ReceiverStream]

    inSequence {
      logsReceiver.expects('startReceiving)(log.name).returning(stream)
      startLine to stopLine foreach { lineNum =>
        stream.expects('receiveLogLine)(log.lines(lineNum).text)
      }
      stream.expects('close)()
    }
  }

  describe("A LogFetcherEngine") {
    it("should extract the correct log lines from all supplied log files") {
      import LineStub.NO_TIME

      val log1 = new LogStub("log1") {
        val lineTimes = Seq(0,1,2,3,NO_TIME,5,6,7,8,9)
      }
      val log2 = new LogStub("log2") {
        val lineTimes = Seq(0,1,2,3,NO_TIME,5,6,7,NO_TIME,9)
      }
      val log3 = new LogStub("log3") {
        val lineTimes = Seq(0,1,2,3,NO_TIME,5,6,7,NO_TIME,NO_TIME)
      }
      val log4 = new LogStub("log4") {
        val lineTimes = Seq(NO_TIME,NO_TIME,NO_TIME,NO_TIME,NO_TIME,5,6,7,8,9)
      }
      val logs = log1 :: log2 :: log3 :: log4 :: Nil

      val startCriterion: Line => Boolean = line => line.time.isDefined && !line.time.get.before(new Date(3))
      val stopCriterion:  Line => Boolean = line => line.time.isDefined && line.time.get.after(new Date(7))
  
      val logsReceiver = mock[LogsReceiver]

      setupExpectations(logsReceiver, log1, startLine = 3, stopLine = 7)
      setupExpectations(logsReceiver, log2, startLine = 3, stopLine = 8)
      setupExpectations(logsReceiver, log3, startLine = 3, stopLine = 9)
      setupExpectations(logsReceiver, log4, startLine = 5, stopLine = 7)
  
      LogFetcherEngine.run(logs, startCriterion, stopCriterion, logsReceiver)
  
      assert(log1.openedLinesObjs.forall(_.closed))
    }
  }
}