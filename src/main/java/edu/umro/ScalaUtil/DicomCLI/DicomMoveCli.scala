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

    val image = cl.hasOption(DicomCliUtil.optionLevelImage.getOpt)
    val series = cl.hasOption(DicomCliUtil.optionLevelSeries.getOpt)
    val patient = cl.hasOption(DicomCliUtil.optionLevelPatient.getOpt)

    (image, series, patient) match {
      case (true, false, false) =>
        argList.foreach(uid => getter.getInstance(uid))
      case (false, true, false) =>
        argList.foreach(serUid => getter.getSeries(serUid))
      case (false, false, true) =>
        val modality = DicomCliUtil.getModality(cl)
        argList.foreach(patient => getter.getPatient(patient, modality))
    }

  }

  private val helpText =
    """
      |C-MOVE one image:
      |
      |    -d C:\MyDir -l localPacs  -r RemotePacs  -I  SOPInstanceUID1  SOPInstanceUID2 ...
      |
      |C-MOVE one series:
      |
      |    -d C:\MyDir -l localPacs  -r RemotePacs  -S  SeriesInstanceUID1  SeriesInstanceUID2 ...
      |
      |C-MOVE one patient:
      |
      |    -c ClientAETitle  -r RemotePacs  <-M Modality> -P  PatientID1  PatientID2 ...
      |
      |Arguments may use either % or * as wildcards, as in the following, which would match
      |any patient ID starting with '$QASRS' :
      |
      |    $QASRS%
      |
      |Modality attribute is not always honored by remote PACS.
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

      println(DicomCliUtil.fmtParameters(cl))
      println

      move(cl)
      Thread.sleep(100)
      System.exit(0)
    }
  }
}
