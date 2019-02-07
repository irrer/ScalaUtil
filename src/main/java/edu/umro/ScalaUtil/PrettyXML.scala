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