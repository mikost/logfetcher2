package name.mikkoostlund.logfetcher2

import java.io.IOException
import java.io.Closeable
import java.io.InputStream
import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import java.io.InputStreamReader
import java.io.BufferedReader
import java.nio.charset.Charset

abstract class ScatteredLog(override val name: String, timeStampParser: TimeStampParser, cs: Charset = Charset.defaultCharset) extends Log {
  if (name == null) throw new NullPointerException("name")
  if (timeStampParser == null) throw new NullPointerException("timeStampParser")
  if (cs == null) throw new NullPointerException("cs")

  val parts: Seq[LogPart]

  case class NormalLogPart(filename: String) extends LogPart {
    override protected
    val createInputStream: () => InputStream = { () =>
      new FileInputStream(filename)
    }
  }

  case class GZIPLogPart(filename: String) extends LogPart {
    override protected
    val createInputStream: () => InputStream = { () =>
      val fis = new FileInputStream(filename)
      try { new GZIPInputStream(fis) }
      catch { case t: Throwable => 
        try { fis.close }
        catch { case ignore: Throwable => }
        throw t
      }
    }
  }

  trait LogPart {
    def open = new LineStream {}

    trait LineStream extends Closeable {
      private[this] lazy 
      val reader: BufferedReader = createReader

      def close(): Unit = reader.close

      def read(): Option[Line] = {
        val nextLineStr = reader.readLine()
        if (nextLineStr == null)
          None
        else
          Some(new Line {
            val text: String = nextLineStr
    	  lazy val time = timeStampParser.parse(text)
          })
      }

      private final
      def createReader: BufferedReader = {
        val is = createInputStream()
        var closeable: Closeable = is
        try {
          val isr = new InputStreamReader(is, cs)
          closeable = isr
          new BufferedReader(isr)
        }
        catch {
          case t: Throwable =>
            try { closeable.close; } 
            catch { case ignored: Throwable => }
            throw t
        }
      }
    }

    protected val createInputStream: () => InputStream
  }

  def open = new Lines {
    var partIter = parts.toIterator
    var currentStream: Option[LogPart#LineStream] = None
    var nextLine: Option[Line] = None
    var isClosed = false

    def close(): Unit = {
      currentStream.foreach { _.close }
      isClosed = true
    }

    def hasNext: Boolean = {
      if (nextLine.isDefined)
        true
      else {
        nextLine = tryGetNextLine()
        nextLine.isDefined
      }
    }

    def next(): Line = {
      if (nextLine.isDefined) {
        val ret = nextLine.get
        nextLine = None
        ret
      }
      else {
        tryGetNextLine() getOrElse { throw new NoSuchElementException }
      }
    }

    private[this]
    def tryGetNextLine(): Option[Line] = {
      def updateCurrentStream(): Unit = {
        currentStream.foreach { _.close }
        if (isClosed) throw new IOException("Log Lines Closed")
        currentStream = 
          if (!partIter.hasNext) None
          else Some(partIter.next.open)
      }

      def read(): Option[Line] = {
        currentStream flatMap { theStream => 
          theStream.read orElse { updateCurrentStream(); read() }
        }
      }

      if (currentStream.isEmpty)
        updateCurrentStream()
      read()
    }
  }
}
