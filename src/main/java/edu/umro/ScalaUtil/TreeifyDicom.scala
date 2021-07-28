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

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import java.util.Date
import java.text.SimpleDateFormat

object TreeifyDicom {

  case class DicomObj(PatientID: String, StudyInstanceUID: String, SeriesInstanceUID: String, Modality: String, file: File, date: Date) {
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss.SSS")
    val dateText = dateFormat.format(date)

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

    def moveTo(seriesDir: File, dicomObj: DicomObj, instanceIndex: Int, outDir: File) = {
      val destFile = new File(seriesDir, Modality + "_" + (instanceIndex + 1).formatted("%03d") + "_" + dateText + ".dcm")
      file.renameTo(destFile)
      print(".")
    }
  }

  /**
   * Search for a usable date+time in an attribute list.  First valid one wins, so the order matters.
   */
  private val dateTimeTagPairs = Seq(
    (TagFromName.TreatmentDate, TagFromName.TreatmentTime),
    (TagFromName.ContentDate, TagFromName.ContentTime),
    (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime),
    (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
    (TagFromName.SeriesDate, TagFromName.SeriesTime),
    (TagFromName.StudyDate, TagFromName.StudyTime),
    (TagFromName.RTPlanDate, TagFromName.RTPlanTime),
    (TagFromName.StructureSetDate, TagFromName.StructureSetTime))

  private def getDateTime(al: AttributeList): Date = {
    dateTimeTagPairs.map(dtp => DicomUtil.getTimeAndDate(al, dtp._1, dtp._2)).flatten.head
  }

  private def readFile(dicomFile: File) = {
    try {
      val al = new AttributeList
      al.read(dicomFile)
      def getAttr(tag: AttributeTag) = new String(al.get(tag).getSingleStringValueOrEmptyString)

      Some(new DicomObj(getAttr(TagFromName.PatientID), getAttr(TagFromName.StudyInstanceUID), getAttr(TagFromName.SeriesInstanceUID), getAttr(TagFromName.Modality), dicomFile, getDateTime(al)))
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
    seriesList.zipWithIndex.map(df => df._1.moveTo(seriesDir, df._1, df._2, outDir))
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

  private def getFilesInTree(file: File): Seq[File] = {
    if (file.isDirectory) {
      file.listFiles.toSeq.map(f => getFilesInTree(f)).flatten
    } else Seq(file)
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

      val fileList = getFilesInTree(inDir)
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
