package edu.umro.ScalaUtil.DicomCLI

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.DicomDictionary
import edu.umro.DicomDict.DicomDict
import edu.umro.ScalaUtil.PACS
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

  private val optionRemotePacs = new Option("r", "remote", true, "AETitle@host:port of the remote PACS.")
  private val optionLocalPacs = new Option("l", "local", true, "AETitle@host:port of the local PACS.")

  val optionLevelImage = new Option("I", "IMAGE", false, "Indicates a single image, slice, or instance.")
  val optionLevelSeries = new Option("S", "SERIES", false, "Indicates an entire series.")
  val optionLevelPatient = new Option("P", "PATIENT", false, "Indicates a entire patient.")

  val optionDir = new Option("d", "directory", true, "Put files in this file system directory.")

  def addClientAETitleOption(options: Options): Unit = {
    optionClient.setRequired(true)
    options.addOption(optionClient)
  }

  def addRemotePacsOption(options: Options): Unit = {
    optionRemotePacs.setRequired(true)
    options.addOption(optionRemotePacs)
  }

  def addLocalPacsOption(options: Options): Unit = {
    optionLocalPacs.setRequired(true)
    options.addOption(optionLocalPacs)
  }

  def addLevelOptions(options: Options): Unit = {
    options.addOption(optionLevelImage)
    options.addOption(optionLevelSeries)
    options.addOption(optionLevelPatient)
  }

  def addDirOption(options: Options): Unit = {
    optionDir.setRequired(true)
    optionDir.setOptionalArg(true)
    options.addOption(optionDir)
  }

  private val optionModality = new Option("M", "MODALITY", true, "If specified, only get series of this modality.")

  def addModalityOption(options: Options): Unit = {
    options.addOption(optionModality)
  }

  def showHelp(options: Options, helpText: String): Unit = {
    val helper = new HelpFormatter()
    helper.printHelp(helpText + "\nUsage:", options)
  }

  private def parseLevelOption(options: Options, command: CommandLine, helpText: String): scala.Option[Option] = {
    val image = command.hasOption(optionLevelImage.getOpt)
    val series = command.hasOption(optionLevelSeries.getOpt)
    val patient = command.hasOption(optionLevelPatient.getOpt)

    (image, series, patient) match {
      case (true, false, false) => Some(optionLevelImage)
      case (false, true, false) => Some(optionLevelSeries)
      case (false, false, true) => Some(optionLevelPatient)
      case _ =>
        val msg = s"Exactly one level option must be given, one of ${optionLevelImage.getOpt}  ${optionLevelSeries.getOpt}  ${optionLevelPatient.getOpt}"
        println(msg)
        showHelp(options, helpText)
        None
    }
  }

  private def parsePacs(text: String): scala.Option[PACS] = {
    try {
      val aeTitle = text.split("@").head
      val hostPort = text.split("@")(1)

      val host = hostPort.split(":").head
      val port = hostPort.split(":")(1).toInt

      Some(new PACS(aeTitle, host, port))
    } catch {
      case _: Throwable => None
    }
  }

  def getLocalPACS(command: CommandLine): PACS = {
    parsePacs(command.getOptionValue(optionLocalPacs.getOpt)).get
  }

  def getRemotePACS(command: CommandLine): PACS = {
    parsePacs(command.getOptionValue(optionRemotePacs.getOpt)).get
  }

  def getModality(command: CommandLine): scala.Option[String] = {
    if (command.hasOption(DicomCliUtil.optionModality.getOpt))
      Some(command.getOptionValue(DicomCliUtil.optionModality.getOpt))
    else
      None
  }

  /**
    * Get the command line arguments and translate all '%' to '*'.  Using '*' in a command line program (e.g. cmd or bash) is
    * awkward due to how they are interpreted.
    *
    * Note that the downside is that it makes it impossible to search for something that actually contains a '%'.
    *
    * @param command Contains arguments.
    * @return List of arguments with '%' converted to '*'.
    */
  def getArgsConverted(command: CommandLine): Seq[String] = {
    command.getArgs.toSeq.map(_.replace('%', '*'))
  }

  def parseOptions(options: Options, args: Array[String], helpText: String): scala.Option[CommandLine] = {
    try {
      val parser = new BasicParser()
      val command = parser.parse(options, args)

      if (command.hasOption(optionLocalPacs.getOpt)) {
        if (parsePacs(command.getOptionValue(optionLocalPacs.getOpt)).isEmpty) {
          println(s"${optionLocalPacs.getOpt} : Local PACS is not correctly defined with AETitle@host:port")
          showHelp(options, helpText)
          System.exit(1)
        }
      }

      if (command.hasOption(optionRemotePacs.getOpt)) {
        if (parsePacs(command.getOptionValue(optionRemotePacs.getOpt)).isEmpty) {
          println(s"${optionRemotePacs.getOpt} : Remote PACS is not correctly defined with AETitle@host:port")
          showHelp(options, helpText)
          System.exit(1)
        }
      }

      if (command.hasOption(optionDir.getOpt)) {
        if (command.getOptionValue(optionDir.getOpt) == null) {
          println(s"Directory must be specified.")
          showHelp(options, helpText)
          System.exit(1)
        }
      }

      if (options.hasOption(optionLevelImage.getOpt))
        parseLevelOption(options, command, helpText)

      Some(command)
    } catch {
      case t: Throwable =>
        println(s"Error parsing command line parameters: ${t.getMessage}")
        showHelp(options, helpText)
        None
    }
  }

  def getClientAETitle(commandLine: CommandLine): String = {
    commandLine.getOptionValue(optionClient.getOpt)
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

    def fmtResult(al: AttributeList): String = {
      tagList.map(tag => fmtAttr(al, tag)).mkString(spacer)
    }

    val text = (Seq(header1, header2, header3) ++ result.map(fmtResult)).mkString("\n")

    text
  }

  /**
    * Format parameters for human consumption.
    * @param cl Command line.
    * @return Human friendly text.
    */
  def fmtParameters(cl: CommandLine): String = {

    def optGet(opt: Option, func: CommandLine => Any, description: String): scala.Option[String] = {
      if (cl.hasOption(opt.getOpt)) {
        val value = func(cl)
        if (value == null)
          Some(s"$description")
        else
          Some(s"$description : ${func(cl)}")
      } else
        None
    }

    val rem = optGet(optionRemotePacs, getRemotePACS, "Remote PACS")

    val loc = optGet(optionLocalPacs, getLocalPACS, "Local PACS")

    val client = optGet(optionClient, _.getOptionValue(optionClient.getOpt), "Local AETitle")

    val mod = optGet(optionModality, _.getOptionValue(optionModality.getOpt), "Modality")

    val img = optGet(optionLevelImage, _.getOptionValue(optionLevelImage.getOpt), "Image level")
    val ser = optGet(optionLevelSeries, _.getOptionValue(optionLevelSeries.getOpt), "Series level")
    val pat = optGet(optionLevelPatient, _.getOptionValue(optionLevelPatient.getOpt), "Patient level")

    val dir = optGet(optionDir, _.getOptionValue(optionDir.getOpt), "Target directory")

    val args = Some("Arguments:  " + getArgsConverted(cl).mkString("    "))

    val text = Seq(rem, loc, mod, img, ser, pat, client, dir, args).flatten.mkString("\n")

    text
  }

}
