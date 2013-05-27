package name.mikkoostlund.logfetcher2

object LogsReceivers {
  def stdout: LogsReceiver =
    new LogsReceiver {
      var _logName: String = _
      def startReceiving(logName: String): ReceiverStream = {
        _logName = logName
        println(logName +" starts...")
        new ReceiverStream {
          def receiveLogLine(logText: String) = println(logName +":"+ logText)
        }
      }
      protected def stopReceiving(): Unit = println(_logName + "...DONE!")
    }
}