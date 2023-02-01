/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package test.java

import edu.umro.ScalaUtil.Logging

object TestLogging extends Logging {
  /*
   *
   * To use your own customized log file, add this to the VM command line:
   *     -Dlog4j2.configurationFile=log4j2.xml
   *
   * If there is a problem with logging, add this to the VM command line:
   *     -Dlog4j2.debug=""
   *
   * Alternately, un-comment this line (or add to calling application) to show more info:
   *     System.setProperty("log4j2.debug", "")
   *
   * In IntelliJ, to set VM options:
   *     (run menu) -->
   *       Edit Configurations -->
   *         Alt-V
   *
   *           or
   *
   *         Modify Options (or Alt-M) -->
   *         Check "Add VM Options" (or, again, Alt-V)
   *
   */

  def main(args: Array[String]): Unit = {
    println("-- Starting ...")
    println("-- log4j2.debug: " + System.getProperty("log4j2.debug") + "  (null means off, empty string means activated)")
    println("-- log4j2.configurationFile: " + System.getProperty("log4j2.configurationFile"))

    // if logging is not working then calling this (can be called from calling application in
    // static section) then logging will work with minimal formatting.
    org.apache.log4j.BasicConfigurator.configure()
    logger.error("logger says error")
    logger.trace("logger says trace")
    logger.warn("logger says warn")
    logger.info("logger says info")
    logger.debug("logger says debug")
    logger.fatal("logger says fatal")
    logger.info("This is\na multi-line\r\nmessage.")
    println("-- Done.")
  }

}
