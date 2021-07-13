package test.java

import edu.umro.ScalaUtil.Logging

object TestLogging extends Logging {
  protected val logNative = org.slf4j.LoggerFactory.getLogger("")
  protected val logNativeClass = org.slf4j.LoggerFactory.getLogger(TestLogging.getClass)
  protected val logNativeUtil = org.slf4j.LoggerFactory.getLogger(edu.umro.ScalaUtil.Util.getClass)

  def main(args: Array[String]): Unit = {
    logger.info("logger says hey")
    logNative.info("logNative says hey")
    logNativeClass.info("logNativeClass says hey")
    logNativeUtil.info("logNativeUtil says hey")
  }

}
