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

package edu.umro.ScalaUtil

import org.apache.logging.log4j.LogManager

/**
  * Log messages.  Extend this trait and use the <code>logger</code> value to log messages.
  *
  * By default, the src/main/resources/log4j2.xml configuration file as built in the jar in the classpath is used.
  * Simply specifying a file in the execution directory will not override this.
  *
  * To use your own customized log file, add this to the VM command line:
  * -Dlog4j2.configurationFile=log4j2.xml
  *
  * If there is a problem with logging, add this to the VM command line:
  * -Dlog4j2.debug=""
  *
  * Alternately, un-comment this line (or add to calling application) to show more info:
  * System.setProperty("log4j2.debug", "")
  *
  * In IntelliJ, to set VM options:
  * (run menu) -->
  * Edit Configurations -->
  * Alt-V
  *
  * or
  *
  * Modify Options (or Alt-M) -->
  * Check "Add VM Options" (or, again, Alt-V)
  *
  * Examples of use in code (also see test/java/TestLogging.scala):
  *
  * <h3>Scala</h3>
  * <pre>
  *
  *     class MyClass extends Logging {
  *         logger.info("Hello from MyClass")
  *
  *         class SubClass {
  *             logger.trace("Hello from SubClass")
  *         }
  *     }
  *
  *     object MyObject extends Logging {
  *         logger.warn("Scary hello from MyObject")
  *     }
  *
  * </pre>
  */
trait Logging {
  // If there is a problem with logging, un-comment this line (or add to calling application) to
  // show more info.  Do this as static code before the first call to logging is made.
  // System.setProperty("log4j2.debug", "")

  protected val logger = LogManager.getLogger(this.getClass)

  /**
    * Format a <code>Throwable</code>.
    *
    * @param throwable Contains description and stack trace.
    *
    * @return Human readable version of <code>Throwable</code> and stack trace.
    */
  def fmtEx(throwable: Throwable): String = {
    val textList = throwable.getStackTrace.map(ste => "\n    " + ste) // convert to text
    textList.foldLeft(throwable.toString)((t, ste) => t + ste) // join as one string
  }
}
