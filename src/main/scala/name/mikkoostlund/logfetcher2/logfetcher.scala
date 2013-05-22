import java.io.FileOutputStream
import java.io.Closeable
import java.util.zip._
import java.util.Date
import java.io.File

trait Line {
  def time: Option[Date]
  def buffer: Array[Byte]
  def length: Int
}

trait Log {
  def name: String
  def lines: Seq[Line]
}

trait LogFetcher {
  private val NEWLINE: Array[Byte] = "\n".getBytes

  def logsToZip(logs: Traversable[Log], startTime: Date, stopTime: Date, zipFileName: String) = {
    withZipOutputStream(zipFileName) { zos =>
      for (log <- logs) {
        val ze = new ZipEntry(log.name);
    	zos.putNextEntry(ze);
        log.lines.
          dropWhile { line => line.time.isEmpty || line.time.get.before(startTime) }.
          takeWhile { line => line.time.isEmpty || line.time.get.before(stopTime)  }.
          foreach { line => 
            zos.write(line.buffer, 0, line.length);
	    zos.write(NEWLINE, 0, 1)
          }
	zos.closeEntry
      }   
    }
  }

  def withZipOutputStream(zipFileName: String)(f: ZipOutputStream => Unit) = {
    val fos = new FileOutputStream(zipFileName)
    var closeable: Closeable = fos
    try {
      val zos0 = new ZipOutputStream(fos)
      closeable = zos0
      f(zos0)
    }
    finally {
      closeable.close
    }
  }
}

trait StringLine extends Line {
  lazy val buffer = line.getBytes("UTF-8")
  lazy val length: Int = buffer.size
  lazy val time: Option[Date] = timeParser(line)
  def timeParser: String => Option[Date]
  def line: String
}

class FileLog(files: Traversable[File]) extends Log {
   def lines: Seq[Line] = ???
   def name: String = ???
}

object Main {
  def main(args: Array[String]): Unit = {

    val myLog1 = new Log {
      class StringLineWithDigitParser(val line: String) extends StringLine {
        val timeParser = { (string: String) => 
          if (string.length == 0) None
          else {
            val timeChar = string.charAt(0)
            if (timeChar < '0' || timeChar > '9')
              None
            else {
              Some(new Date(Integer.parseInt(timeChar.toString)))
            }
          }
        }
      }
      def name = "urp"
      def lines = 
        new StringLineWithDigitParser("1 One")   :: 
        new StringLineWithDigitParser("2 Two")   :: 
        new StringLineWithDigitParser("3 Three") :: 
        new StringLineWithDigitParser("4 Four")  :: Nil
    }

    val myLog2 = new Log {
      class StringLineWithAlfaParser(val line: String) extends StringLine {
        val timeParser = { (string: String) => 
          if (string.length == 0) None
          else {
            val timeChar = string.charAt(0)
            if (timeChar < 'A' || timeChar > 'J')
              None
            else {
              val converted: Char = ('0' + (timeChar - 'A')).asInstanceOf[Char]
              Some(new Date(Integer.parseInt(converted.toString)))
            }
          }
        }
      }
      def name = "urp2"
      def lines = 
        new StringLineWithAlfaParser("A ay")  :: 
        new StringLineWithAlfaParser("B bee") :: 
        new StringLineWithAlfaParser("X ex This line is included if B is included, because it has an invalid timestamp") :: 
        new StringLineWithAlfaParser("C see") :: 
        new StringLineWithAlfaParser("D dee") :: 
        new StringLineWithAlfaParser("E ee") :: Nil
    }

    val startTime = new Date(1)
    val stopTime = new Date(3)
    val logs = myLog1 :: myLog2 :: Nil

    new LogFetcher {}.logsToZip(logs, startTime, stopTime, "rune.zip")
  }
}