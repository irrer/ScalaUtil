package edu.umro.ScalaUtil.DicomCLI

import edu.umro.ScalaUtil.dicomCFind.DicomCFindInstancesForSeries
import edu.umro.ScalaUtil.dicomCFind.DicomCFindPatientList
import edu.umro.ScalaUtil.dicomCFind.DicomCFindSeriesForPatient
import org.apache.commons.cli.CommandLine

/**
  * Provide a CLI (command line interface) for the DICOM C-FIND library.
  */
object DicomFindCli {

  private def find(cl: CommandLine): Unit = {

    val remotePACS = DicomCliUtil.getRemotePACS(cl)

    val localAETitle = DicomCliUtil.getClientAETitle(cl)

    val argList = DicomCliUtil.getArgsConverted(cl)

    val image = cl.hasOption(DicomCliUtil.optionLevelImage.getOpt)
    val series = cl.hasOption(DicomCliUtil.optionLevelSeries.getOpt)
    val patient = cl.hasOption(DicomCliUtil.optionLevelPatient.getOpt)

    (image, series, patient) match {
      case (true, false, false) =>
        val finder = new DicomCFindInstancesForSeries(localAETitle, remotePACS)
        argList.foreach(uid => {
          val list = finder.findInstanceListForSeries(uid)
          println(DicomCliUtil.findResultToText(list))
        })

      case (false, true, false) =>
        val modality = DicomCliUtil.getModality(cl)
        val finder = new DicomCFindSeriesForPatient(localAETitle, remotePACS)
        argList.foreach(serUid => {
          val list = finder.findSeriesForPatient(serUid, modality)
          println(DicomCliUtil.findResultToText(list))
        })

      case (false, false, true) =>
        val finder = new DicomCFindPatientList(localAETitle, remotePACS)
        argList.foreach(p => {
          val list = finder.findPatientList(p)
          println(DicomCliUtil.findResultToText(list))
        })
    }

  }

  private val helpText =
    """
      |Utility for performing C-FIND from the command line.
      |
      |List image in series:
      |
      |    -c ClientAETitle  -r RemotePacs  -I  SeriesInstanceUID
      |
      |List Series for patient:
      |
      |    -c ClientAETitle  -r RemotePacs  [-M Modality ]  -S  PatientID1  PatientID2  ...
      |
      |List Patients.  May contain either
      |
      |    -c ClientAETitle  -r RemotePacs  -P  PatientID1  PatientID2 ...
      |
      |Arguments may use either % or * as wildcards, as in the following, which would match
      |any patient ID starting with '$QASRS' :
      |
      |    $QASRS%
      |
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    import DicomCliUtil._
    import org.apache.commons.cli.Options

    val options = new Options()

    DicomCliUtil.addRemotePacsOption(options)
    DicomCliUtil.addClientAETitleOption(options)
    DicomCliUtil.addLevelOptions(options)
    DicomCliUtil.addModalityOption(options)

    val commandLine = parseOptions(options, args, helpText)

    if (commandLine.isEmpty)
      System.exit(1)
    else {
      val cl = commandLine.get

      println(DicomCliUtil.fmtParameters(cl))
      println

      find(cl)
      Thread.sleep(100)
      System.exit(1)
    }
  }
}
