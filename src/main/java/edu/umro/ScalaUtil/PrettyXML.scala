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

import scala.xml.PrettyPrinter
import scala.xml.Node
import java.io.File
import scala.xml.XML
import java.io.FileOutputStream

/**
 * Overwrite the given XML files with formatted XML.
 */
object PrettyXML {

  def xmlToText(node: Node): String = new PrettyPrinter(1024, 2).format(node)

  private def makePretty(fileName: String): Unit = {
    try {
      println("Processing file " + fileName)
      val file = new File(fileName)
      val text = xmlToText(XML.loadFile(file))
      val fos = new FileOutputStream(file)
      fos.write(text.getBytes)
      fos.close
      System.exit(0)
    } catch {
      case t: Throwable =>
        println("Unable to process file " + fileName + " : " + t.getMessage)
        System.exit(7)
    }

  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) println("Usage: PrettyXML file1.xml file2.xml ...\n\nNote that files will be overwritten.  Non-XML files will be ignored.")
    else args.map(name => makePretty(name))
  }

}