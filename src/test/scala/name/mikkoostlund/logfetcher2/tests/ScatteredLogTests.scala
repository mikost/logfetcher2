package name.mikkoostlund.logfetcher2.tests

import org.scalatest.FunSpec
import org.scalamock.scalatest.proxy.MockFactory
import name.mikkoostlund.logfetcher2.ScatteredLog
import name.mikkoostlund.logfetcher2.TimeStampParser

class ScatteredLogTests extends FunSpec with MockFactory {

  val timeStampParser = mock[TimeStampParser]
  
  def getLog = new ScatteredLog("test", timeStampParser) {
    val part = mock[LogPart]
    val lineStream = mock[part.LineStream]
    val parts = part :: Nil

  }

  import language.reflectiveCalls 
  
  describe("A ScatteredLog that comprises a single part, which is empty,") {
    it("should, when its open method is called, return an empty Lines object") {
      val log = getLog
      log.part.expects('open)().returning(log.lineStream)
      log.lineStream.expects('read)().returning(None)
      log.lineStream.expects('close)()
      val lines = log.open
      assert(!lines.hasNext)
    }
  }
}