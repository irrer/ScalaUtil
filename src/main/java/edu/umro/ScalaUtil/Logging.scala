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

import org.slf4j.Logger

/**
 * Log messages.  Extend this trait and use the <code>logger</code> value to log messages.
 *
 * To bind slf4j to log4j, the following VM properties should be set, as in:
 *
 * <pre>
 *
 *     -Djava.util.logging.manager=org.apache.logging.log4j.jul.LogManager
 *     -Dlog4j2.configurationFile=src\main\resources\log4j2.xml
 *
 * </pre>
 *
 *
 *
 * Examples of use:
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
  protected lazy val logger: Logger = org.slf4j.LoggerFactory.getLogger(this.getClass)

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