package edu.umro.ScalaUtil

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.DicomDictionary
import edu.umro.DicomDict.DicomDict
import org.apache.commons.cli.BasicParser
import org.apache.commons.cli.CommandLine
import org.apache.commons.cli.HelpFormatter
import org.apache.commons.cli.Option
import org.apache.commons.cli.Options

/**
  * Provide support for processing common command line options.
  */
object DicomCliUtil {

  private val optionClient = new Option("c", "client", true, "AE title of the client (this program).")
  private val optionServerAETitle = new Option("s", "server", true, "AE title of the server.")
  private val optionHost = new Option("h", "host", true, "Host name or IP address of server.")
  private val optionPort = new Option("p", "port", true, "DICOM Port number on server.")

  def addClientAETitleOption(options: Options): Unit = {
    optionClient.setRequired(true)
    options.addOption(optionClient)
  }

  def addServerOptions(options: Options): Unit = {

    optionServerAETitle.setRequired(true)
    options.addOption(optionServerAETitle)

    optionHost.setRequired(true)
    options.addOption(optionHost)

    optionPort.setRequired(true)
    options.addOption(optionPort)
  }

  def showHelp(options: Options): Unit = {
    val helper = new HelpFormatter()
    helper.printHelp("Usage:", options)
  }

  def parseOptions(options: Options, args: Array[String]): scala.Option[CommandLine] = {
    try {
      val parser = new BasicParser()
      val command = parser.parse(options, args)

      if (command.hasOption(optionPort.getOpt)) {
        val portText = command.getOptionValue(optionPort.getOpt)
        val port: Int =
          try {
            portText.toInt
          } catch {
            case _: Throwable =>
              throw new RuntimeException(s"Invalid port number $portText specified.  Must be an integer greater than 0.")
          }
        if (port < 1)
          throw new RuntimeException(s"Invalid port number $port specified.  Must be an integer greater than 0.")
      }

      Some(command)
    } catch {
      case t: Throwable =>
        println(s"Error parsing command line parameters: ${t.getMessage}")
        showHelp(options)
        None
    }
  }

  def getClientAETitle(commandLine: CommandLine): String = {
    commandLine.getOptionValue(optionClient.getOpt)
  }

  def getServerPACS(commandLine: CommandLine): PACS = {
    val aeTitle = commandLine.getOptionValue(optionServerAETitle.getOpt)
    val host = commandLine.getOptionValue(optionHost.getOpt)
    val port = commandLine.getOptionValue(optionPort.getOpt).toInt
    new PACS(aeTitle, host, port)
  }

  def findResultToText(result: Seq[AttributeList]): String = {

    val maxLength = 70

    val maxFmt = s"%-${maxLength}s"
    def fmt(text: String): String = maxFmt.format(text)

    val spaces = fmt(" ")

    def prefix(text: String, length: Int): String = {
      text.substring(0, Math.min(text.length(), length))
    }

    val spacer = "  "

    val ellipsis = "..."

    def tagName(tag: AttributeTag): String = {
      Seq(DicomDictionary.StandardDictionary.getNameFromTag(tag), DicomDict.dict.getNameFromTag(tag)).head
    }

    // a list of all distinct tags, sorted by group and element.
    val tagList = result.flatMap(al => al.keySet().toArray.map(_.asInstanceOf[AttributeTag])).distinct.sortBy(tag => (tag.getGroup << 16) + tag.getElement)

    def attrToText(attr: Attribute): String = {
      val text = attr.getSingleStringValueOrNull
      if (text == null)
        "<null>"
      else
        text
    }

    def maxTagSize(tag: AttributeTag): Int = {
      val maxTextLength = result.map(al => al.get(tag)).filter(_ != null).map(attrToText).maxBy(_.length).length
      val x = Math.max(maxTextLength, tagName(tag).length)
      Math.min(x, maxLength) // do not allow really long values
    }

    val tagSizeMap = tagList.map(tag => (tag, maxTagSize(tag))).toMap

    def fmtAttr(al: AttributeList, tag: AttributeTag): String = {
      val attr = al.get(tag)
      val text = {
        if (attr == null) {
          "<NA>"
        } else {
          val text = attr.getSingleStringValueOrNull()
          if (text == null) {
            "<null>"
          } else {
            val t = text.trim
            if (t.length <= tagSizeMap(attr.getTag))
              prefix(t + spaces, maxLength)
            else {
              prefix(t, maxLength - ellipsis.length) + ellipsis
            }
          }
        }
      }

      val t = prefix(text + spaces, tagSizeMap(tag))
      t
    }

    def fmtResult(al: AttributeList): String = {
      tagList.map(tag => fmtAttr(al, tag)).mkString(spacer)
    }

    val header1 = {
      tagList
        .map(tag => {
          prefix(tagName(tag) + spaces, maxTagSize(tag))
        })
        .mkString(spacer)
    }

    val header2 = {
      tagList
        .map(tag => {
          val t1 = "%04x".format(tag.getGroup)
          val t2 = "%04x".format(tag.getElement)
          prefix(s"$t1,$t2$spaces", maxTagSize(tag))
        })
        .mkString(spacer)
    }

    val header3 = {
      val line = (0 until maxLength).map(_ => "-").mkString("")
      tagList
        .map(tag => prefix(line, maxTagSize(tag)))
        .mkString(spacer)
    }

    val text = (Seq(header1, header2, header3) ++ result.map(fmtResult)).mkString("\n")

    text
  }

}
