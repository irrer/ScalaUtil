package edu.umro.ScalaUtil

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag

object TreeifyDicom {

  case class DicomObj(PatientID: String, StudyInstanceUID: String, SeriesInstanceUID: String, Modality: String, file: File) {

    def makeSeriesDir(index: Int, studyDir: File): File = {
      val seriesDir = new File(studyDir, Modality + "_" + index.formatted("%02d"))
      if (seriesDir.isDirectory) makeSeriesDir(index + 1, studyDir)
      else {
        seriesDir.mkdirs
        seriesDir
      }
    }

    def getSeriesDir(studyIndex: Int, outDir: File) = {
      val patDir = new File(outDir, PatientID)
      val studyDir = new File(patDir, "Study_" + (studyIndex + 1))
      val seriesDir = makeSeriesDir(1, studyDir)
      seriesDir.mkdirs
      seriesDir
    }

    def moveTo(seriesDir: File, instanceIndex: Int, outDir: File) = {
      val destFile = new File(seriesDir, Modality + "_" + (instanceIndex + 1).formatted("%03d") + ".dcm")
      file.renameTo(destFile)
      print(".")
    }
  }

  private def readFile(dicomFile: File) = {
    try {
      val al = new AttributeList
      al.read(dicomFile)
      def getAttr(tag: AttributeTag) = new String(al.get(tag).getSingleStringValueOrEmptyString)

      Some(new DicomObj(getAttr(TagFromName.PatientID), getAttr(TagFromName.StudyInstanceUID), getAttr(TagFromName.SeriesInstanceUID), getAttr(TagFromName.Modality), dicomFile))
    } catch {
      case t: Throwable => {
        println("Unable to read file as DICOM.  Ignoring: " + dicomFile.getAbsolutePath)
        None
      }
    }
  }

  private def moveSeries(seriesList: Seq[DicomObj], studyIndex: Int, seriesIndex: Int, outDir: File, regWithCBCT: Boolean) = {
    val seriesDir = seriesList.head.getSeriesDir(studyIndex, outDir)
    print("Putting series in " + seriesDir.getAbsolutePath + "  ")
    seriesList.zipWithIndex.map(df => df._1.moveTo(seriesDir, df._2, outDir))
    println
  }

  private def moveStudy(study: Seq[DicomObj], studyIndex: Int, outDir: File, regWithCBCT: Boolean) = {
    val seriesList = study.groupBy(_.SeriesInstanceUID).map(kv => kv._2).toSeq
    seriesList.zipWithIndex.map(series => moveSeries(series._1, studyIndex, series._2, outDir, regWithCBCT))
  }

  private def movePatient(patient: Seq[DicomObj], outDir: File, regWithCBCT: Boolean) = {
    val studyList = patient.groupBy(_.StudyInstanceUID).map(kv => kv._2).toSeq
    studyList.zipWithIndex.map(study => moveStudy(study._1, study._2, outDir, regWithCBCT))
  }

  def main(args: Array[String]): Unit = {
    try {
      val start = System.currentTimeMillis
      val regWithCBCT = System.getProperty("REGwithCBCT") != null
      println("regWithCBCT: " + regWithCBCT)
      if (args.size != 2) {
        println("Usage: treeify inputdir outputdir")
        System.exit(1)
      }
      val inDir = new File(args(0))
      val outDir = new File(args(1))
      val fileList = inDir.listFiles.toSeq
      println("Number of files to process: " + fileList.size)
      val dicomFileList = fileList.map(f => readFile(f)).flatten
      println("Number of DICOM files to process: " + dicomFileList.size)
      val patientList = dicomFileList.groupBy(_.PatientID).map(kv => kv._2).toSeq

      patientList.map(patient => movePatient(patient, outDir, regWithCBCT))
      println("\nDone.  Elapsed ms: " + (System.currentTimeMillis - start))
    } catch {
      case t: Throwable =>
        t.printStackTrace
    }
  }

}
