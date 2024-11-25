package edu.umro.ScalaUtil.DicomCLI

import edu.umro.ScalaUtil.dicomCMove.DicomCMoveGetter
import edu.umro.ScalaUtil.dicomCMove.DicomCMoveReceiver
import org.apache.commons.cli.CommandLine

import java.io.File

/**
  * Provide a CLI (command line interface) for the DICOM C-MOVE library.
  */
object DicomMoveCli {

  private def move(cl: CommandLine): Unit = {

    val dir = {
      val dirName: String = cl.getOptionValue(DicomCliUtil.optionDir.getOpt)
      new File(dirName)
    }

    val receiver = DicomCMoveReceiver(dir, DicomCliUtil.getLocalPACS(cl))

    val getter = new DicomCMoveGetter(DicomCliUtil.getRemotePACS(cl), receiver)

    val argList = cl.getArgs.toSeq

    val image = cl.getOptionValue(DicomCliUtil.optionLevelImage.getOpt)
    val series = cl.getOptionValue(DicomCliUtil.optionLevelSeries.getOpt)
    val patient = cl.getOptionValue(DicomCliUtil.optionLevelPatient.getOpt)

    (image, series, patient) match {
      case (_, null, null) =>
        argList.foreach(uid => getter.getInstance(uid))
      case (null, _, null) =>
        argList.foreach(serUid => getter.getSeries(serUid))
      case (null, null, _) =>
        val modality = DicomCliUtil.getModality(cl)
        argList.foreach(patient => getter.getPatient(patient, modality))
    }

  }

  private val helpText =
    """
      |
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    import DicomCliUtil._
    import org.apache.commons.cli.Options

    val options = new Options()

    DicomCliUtil.addRemotePacsOption(options)
    DicomCliUtil.addLocalPacsOption(options)
    DicomCliUtil.addLevelOptions(options)
    DicomCliUtil.addModalityOption(options)
    DicomCliUtil.addDirOption(options)

    val commandLine = parseOptions(options, args, helpText)

    if (commandLine.isEmpty)
      System.exit(1)
    else {
      val cl = commandLine.get
      move(cl)
      Thread.sleep(100)
      System.exit(1)
    }
  }
}
