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
import edu.umro.util.Utility
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Logging
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TransferSyntax
import java.util.Date
import com.pixelmed.dicom.FileMetaInformation
import com.pixelmed.dicom.Attribute
import java.text.SimpleDateFormat
import java.util.Calendar
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy

/**
 * Copy a directory of DICOM files into a new file tree structured by the frame of reference of
 * files.  The new files are given names composed of their content date, frame of reference, and
 * modality.
 *
 * This tool is intended for cases when a directory (may be one or more layers deep) contains a
 * jumble of many (like 10's of thousands) DICOM files.
 *
 * If a DICOM file occurs more than once in the source tree (identified by SOPInstanceUID) then
 * only one copy will be made in the destination tree.
 *
 * The destination directory with be the same name as the original, with "dag" appended, as
 * in:   patientX --> patientXdag
 *
 * File tree:
 *
 * Files and directories have a timestamp of the form:
 * [year]-[month]-[day]_[hour]-[minue]-[sec]-[millisecond]
 *
 * Example frame of reference directory:
 *     FrmofRef_2019-08-26_08-50-43-523_010  // Directory containing everything with this frame of reference followed by arbitrary unique frame of reference index
 *
 * Example non-image DICOM file:
 *     RTSTRUCT_010_001_2016-02-14_14-21-22-000.dcm  // [modality]_[frame of ref index]_[arbitrary unique index][date].[DICOM file suffix]
 *
 * Example image directory:
 *     CT_OBI_Cone_beam_CT_2019-08-26_08-50-43-523_001  // [modality]_[manufacturer model of machine]_[arbitrary index to make sure directory has a unique name]
 */

object DicomFOR extends Logging {

  private val frameOfRefSet = scala.collection.mutable.Set[String]()

  val calendar = Calendar.getInstance
  val tzOffset_ms = calendar.get(Calendar.ZONE_OFFSET) + calendar.get(Calendar.DST_OFFSET)
  val dateFormat = new SimpleDateFormat("_yyyy-MM-dd_HH-mm-ss-SSS")

  private val future = new SimpleDateFormat("yyyyMMddHHMM").parse("210001011111")

  private var dcmFlCount = 0

  def textDateOf(alList: Seq[DcmFl]): String = {
    try {
      val maxDate = new Date(alList.map(ct => ct.date.getTime).max)
      dateFormat.format(new Date(maxDate.getTime + tzOffset_ms))
    } catch {
      case t: Throwable => {
        if (alList.nonEmpty) {
          Trace.trace("bad date for list of als")
        }
        dateFormat.format(new Date(future.getTime + tzOffset_ms))
      }
    }
  }

  private val doneSet = scala.collection.mutable.Set[String]()

  private val uidMap = scala.collection.mutable.HashMap[String, String]()

  private def getNewUid(oldUid: String): String = {
    if (uidMap.contains(oldUid)) uidMap(oldUid)
    else {
      val newUid = UMROGUID.getUID
      uidMap.put(oldUid, newUid)
      newUid
    }
  }

  private def updateAttrUid(attr: Attribute) = {
    val oldUid = attr.getSingleStringValueOrEmptyString
    val newUid = getNewUid(oldUid)
    attr.removeValues
    attr.addValue(newUid)
  }

  val transferSyntax = TransferSyntax.ImplicitVRLittleEndian

  private def readFile(file: File) = {
    val al = new AttributeList
    //Trace.trace("reading file: " + file.getAbsolutePath)
    al.read(file)
    al
  }

  private def writeFile(al: AttributeList, file: File) = {
    FileMetaInformation.addFileMetaInformation(al, transferSyntax, "JimIrrer")
    DicomUtil.writeAttributeListToFile(al, file, "JimIrrer")
    //Trace.trace("Created " + file.getAbsolutePath)
  }

  private def seriesOf(al: AttributeList) = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  private def positionOf(al: AttributeList) = {
    val ipp = al.get(TagFromName.SliceLocation)
    if (ipp == null) 0 else ipp.getDoubleValues.head
  }

  private def modalityOf(al: AttributeList) = al.get(TagFromName.Modality).getSingleStringValueOrEmptyString

  private def sopOf(al: AttributeList) = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString

  private def seriesUidOf(al: AttributeList) = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString

  private def manufModOf(al: AttributeList) = {
    val dflt = "unknown"
    try {
      al.get(TagFromName.ManufacturerModelName).getSingleStringValueOrDefault(dflt)
    } catch {
      case t: Throwable => dflt
    }
  }

  private def frameOfRefOf(al: AttributeList): String = {
    val list = DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).map(at => at.getSingleStringValueOrEmptyString)

    if (modalityOf(al).equals("REG") && (list.size > 1)) {
      val mainFrmOfRef = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
      list.filterNot(frmOfRef => frmOfRef.eq(mainFrmOfRef)).head
    } else {
      if (list.isEmpty)
        "none"
      else
        list.head
    }
  }

  private def refPlanOf(al: AttributeList): String = {
    if (al.get(DicomUtil.dictionary.getTagFromName("ReferencedRTPlanSequence")) != null) {
      val refSeq = DicomUtil.seqToAttr(al, TagByName.ReferencedRTPlanSequence).head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
      refSeq
    } else ""
  }

  def dateOfAl(al: AttributeList): Date = {
    val dateTimeTagPairList = List(
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagByName.CreationDate, TagByName.CreationTime),
      (TagFromName.StudyDate, TagFromName.StudyTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    def get(dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
      try {
        val d = DicomUtil.dicomDateFormat.parse(al.get(dateTag).getSingleStringValueOrNull)
        val t = {
          val text: String = al.get(timeTag).getSingleStringValueOrNull
          new Date(DicomUtil.parseDicomTime(text).get)
        }

        Some(new Date(d.getTime + t.getTime))

      } catch {
        case t: Throwable => {
          None
        }
      }
    }

    val list = dateTimeTagPairList.map(dt => get(dt._1, dt._2))
    list.flatten.head
  }

  val tagsOfInterest = Seq(
    TagFromName.AcquisitionDate,
    TagFromName.AcquisitionTime,
    TagFromName.ContentDate,
    TagFromName.ContentTime,
    TagByName.CreationDate,
    TagByName.CreationTime,
    TagFromName.ImagePositionPatient,
    TagFromName.SliceLocation,
    TagFromName.InstanceCreationDate,
    TagFromName.InstanceCreationTime,
    TagFromName.ManufacturerModelName,
    TagFromName.Modality,
    TagFromName.SeriesDate,
    TagFromName.SeriesInstanceUID,
    TagFromName.SeriesTime,
    TagFromName.SOPInstanceUID,
    TagFromName.StudyDate,
    TagFromName.StudyTime)

  def tagToLong(t: AttributeTag): Long = {
    (t.getGroup.toLong << 16) + t.getElement
  }

  val lastTagOfInterestAsLong = tagToLong(tagsOfInterest.maxBy(tagToLong))

  private def readPartial(file: File, lastTagAsLong: Long = lastTagOfInterestAsLong): AttributeList = {

    var savedAttrList = new AttributeList
    class ReadStrategy extends ReadTerminationStrategy {
      override def terminate(al: AttributeList, tag: AttributeTag, bytesRead: Long): Boolean = {
        savedAttrList = al
        tagToLong(tag) > lastTagAsLong
      }
    }

    val readStrategy = new ReadStrategy

    try {
      (new AttributeList).read(file, readStrategy)
    } catch {
      case t: Throwable => ;
    }
    savedAttrList
  }

  var timeToShowProgress = System.currentTimeMillis + 1000

  private class DcmFl(f: File) {

    val al: AttributeList = {
      val partial = readPartial(f)
      val isRtstruct = partial.get(TagFromName.Modality).getSingleStringValueOrEmptyString.equals("RTSTRUCT")
      if (isRtstruct)
        readPartial(f, tagToLong(TagByName.StructureSetROISequence))
      else
        partial
    }

    val file = f
    val modality = new String(modalityOf(al))
    val sop = new String(sopOf(al))
    val seriesUid = new String(seriesUidOf(al))
    val frameOfRef = new String(frameOfRefOf(al))
    if (frameOfRef.equals("none") && modality.equals("RTSTRUCT")) {
      Trace.trace("No frame of reference for RTSTRUCT " + file.getAbsolutePath)
    }
    val referencedPlan = new String(refPlanOf(al))
    val manufacturerModelName = new String(manufModOf(al))

    val position = positionOf(al)
    val date = {
      try {
        dateOfAl(al)
      } catch {
        case t: Throwable => {
          future
        }
      }
    }
    val dateText = dateFormat.format(new Date(date.getTime + tzOffset_ms))

    def copyTo(dest: File) = {
      val data = FileUtil.readBinaryFile(file).right.get
      dest.getParentFile.mkdirs
      FileUtil.writeBinaryFile(dest, data)
    }

    dcmFlCount = dcmFlCount + 1
    val now = System.currentTimeMillis
    if (now > timeToShowProgress) {
      Trace.trace("Files read: " + dcmFlCount)
      timeToShowProgress = now + 1000
      if ((dcmFlCount % 1000) == 0) {
        Runtime.getRuntime.gc
        Thread.sleep(50)
      }
    }

  }

  def resolution(f: File): Int = {
    val al = readFile(f)
    al.get(TagFromName.Rows).getIntegerValues.head * al.get(TagFromName.Columns).getIntegerValues.head
  }

  private def saveRtstruct(rtstructDF: DcmFl, outDir: File, frameOfRefIndex: Int, rtstructIndex: Int, imageSeriesList: Seq[Seq[DcmFl]]) = {
    val destFile = new File(outDir, "RTSTRUCT_" + fmt(frameOfRefIndex) + "_" + fmt(rtstructIndex) + rtstructDF.dateText + ".dcm")
    Trace.trace("Saving RTSTRUCT " + destFile.getAbsolutePath)
    rtstructDF.copyTo(destFile)
  }

  def fmt(i: Int) = i.formatted("%03d")

  private def saveImageSeries(imageSeries: Seq[DcmFl], index: Int, outDir: File) = {
    val modality = imageSeries.head.modality
    if (modality.equals("MR"))
      Trace.trace
    val manfName = imageSeries.head.manufacturerModelName.replaceAll("[^a-zA-Z0-9]", "_")
    val imageDirName = (modality + "_" + manfName + "_" + textDateOf(imageSeries) + "_" + fmt(index)).replaceAll("___*", "_")
    val imageDir = new File(outDir, imageDirName)
    Trace.trace("Saving " + modality + " series " + imageDir.getAbsolutePath)
    imageDir.mkdirs
    imageSeries.zipWithIndex.map(imgIdx => {
      val dest = new File(imageDir, modality + "-" + fmt(imgIdx._2 + 1) + ".dcm")
      imgIdx._1.copyTo(dest)
    })
  }

  private def saveImageSeriesList(imageSeriesList: Seq[Seq[DcmFl]], index: Int, outDir: File) = {
    imageSeriesList.zipWithIndex.map(si => saveImageSeries(si._1, si._2 + 1, outDir))
  }

  private def saveRtplan(rtplanDM: DcmFl, outDir: File, frmOfRefIndex: Int, planIndex: Int) = {
    val dest = new File(outDir, "RTPLAN_" + rtplanDM.dateText + "_" + fmt(planIndex) + ".dcm")
    Trace.trace("Saving RTPLAN " + dest.getAbsolutePath)
    rtplanDM.copyTo(dest)
  }

  private def saveRtimage(rtimageDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtimageIndex: Int) = {
    val dest = new File(outDir, "RTIMAGE" + rtimageDM.dateText + "_" + fmt(rtimageIndex) + ".dcm")
    Trace.trace("Saving RTIMAGE " + dest.getAbsolutePath)
    rtimageDM.copyTo(dest)
  }

  private def saveReg(regDM: DcmFl, outDir: File, frmOfRefIndex: Int, regIndex: Int) = {
    val dest = new File(outDir, "REG" + regDM.dateText + "_" + fmt(regIndex) + ".dcm")
    Trace.trace("Saving REG " + dest.getAbsolutePath)
    regDM.copyTo(dest)
  }

  private def saveRtdose(rtdoseDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtdoseIndex: Int) = {
    val dest = new File(outDir, "RTDOSE" + rtdoseDM.dateText + "_" + fmt(rtdoseIndex) + ".dcm")
    Trace.trace("Saving RTDOSE " + dest.getAbsolutePath)
    rtdoseDM.copyTo(dest)
  }

  private def saveRtrecord(regDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtrecordIndex: Int) = {
    val dest = new File(outDir, "RTRECORD_" + regDM.dateText + "_" + fmt(rtrecordIndex) + ".dcm")
    Trace.trace("Saving RTRECORD " + dest.getAbsolutePath)
    regDM.copyTo(dest)
  }

  private def getByRefPlan(frmOfRef: String, rtplanList: Seq[DcmFl], allDcm: Seq[DcmFl]): Seq[DcmFl] = {
    val rtplanSopList = rtplanList.map(rtplan => rtplan.sop)
    allDcm.filter(d => rtplanSopList.contains(d.referencedPlan))
  }

  private def fix(frmOfRef: String, frmOfRefIndex: Int, allDcm: Seq[DcmFl], outDir: File) = {
    Trace.trace("Processing frame of ref: " + frmOfRefIndex)
    val frmOfRefList = allDcm.filter(d => d.frameOfRef.equals(frmOfRef))
    val imageSeriesList = frmOfRefList.filter(d => d.modality.equals("CT") || d.modality.equals("MR")).
      groupBy(_.seriesUid).
      map(ss => ss._2.sortBy(d => d.position)).
      toSeq.
      sortBy(s => resolution(s.head.file))

    val frmOfRefDirDate = {
      (imageSeriesList.nonEmpty, frmOfRefList.find(f => f.modality.equals("RTIMAGE"))) match {
        case (true, _) => textDateOf(imageSeriesList.head)
        case (_, Some(dcmFl)) => textDateOf(frmOfRefList.filter(d => d.modality.equals("RTIMAGE")))
        case _ => textDateOf(frmOfRefList)
      }
    }

    val frmOfRefDir = new File(outDir, "FrmofRef" + frmOfRefDirDate + "_" + fmt(frmOfRefIndex))
    if (imageSeriesList.nonEmpty) saveImageSeriesList(imageSeriesList, frmOfRefIndex, frmOfRefDir)

    val rtstructList = frmOfRefList.filter(d => d.modality.equals("RTSTRUCT"))
    // private def fixRtstruct(rtstructDF: DcmFl, outDir : File, index: Int, frameOfRefIndex: Int, rtstructIndex: Int, ctList: Seq[DcmFl]) = {
    rtstructList.zipWithIndex.map(di => saveRtstruct(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1, imageSeriesList))

    val rtplanList = allDcm.filter(d => d.frameOfRef.equals(frmOfRef) && d.modality.equals("RTPLAN"))
    // private def fixRtstruct(rtstructDF: DcmFl, outDir : File, index: Int, frameOfRefIndex: Int, rtstructIndex: Int, ctList: Seq[DcmFl]) = {
    rtplanList.zipWithIndex.map(di => saveRtplan(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val rtimageList = frmOfRefList.filter(d => d.modality.equals("RTIMAGE"))
    rtimageList.zipWithIndex.map(di => saveRtimage(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val regList = frmOfRefList.filter(d => d.modality.equals("REG"))
    regList.zipWithIndex.map(di => saveReg(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val refRtplanList = getByRefPlan(frmOfRef, rtplanList, allDcm).filter(d => d.modality.equals("RTRECORD") || d.modality.equals("RTDOSE"))

    val rtdoseList = refRtplanList.filter(d => d.modality.equals("RTDOSE"))
    rtdoseList.zipWithIndex.map(di => saveRtdose(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))

    val rtrecordList = refRtplanList.filter(d => d.modality.equals("RTRECORD"))
    rtrecordList.zipWithIndex.map(di => saveRtrecord(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))
  }

  private def saveOther(allDcm: Seq[DcmFl], outDir: File) = {
    val noFrameOfRef = allDcm.filter(d => d.frameOfRef.size <= 5)
    val byModality = allDcm.filter(d =>
      (!d.modality.equals("CT")) &&
        (!d.modality.equals("MR")) &&
        (!d.modality.equals("RTSTRUCT")) &&
        (!d.modality.equals("RTIMAGE")) &&
        (!d.modality.equals("REG")) &&
        (!d.modality.equals("RTRECORD")) &&
        (!d.modality.equals("RTDOSE")) &&
        (!d.modality.equals("RTPLAN")))

    def saveSeries(series: Seq[DcmFl], seriesIndex: Int) = {
      val seriesDate = textDateOf(series)
      if (series.size == 1) {
        val dest = new File(outDir, series.head.modality + seriesDate + "_" + fmt(seriesIndex) + ".dcm")
        series.head.copyTo(dest)
      } else {
        val seriesDir = new File(outDir, series.head.modality + seriesDate + "_" + fmt(seriesIndex))
        seriesDir.mkdirs
        series.zipWithIndex.map(di => {
          val dest = new File(seriesDir, series.head.modality + seriesDate + "_" + fmt(di._2 + 1) + ".dcm")
          di._1.copyTo(dest)
        })
      }
    }

    (noFrameOfRef ++ byModality).
      groupBy(_.seriesUid).
      map(ss => ss._2).
      zipWithIndex.map(ssi => saveSeries(ssi._1, ssi._2 + 1))
  }

  /**
   * Safely get a list of files in a directory.  On failure, return an empty list.
   */
  def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case t: Throwable => List[File]()
    }
  }

  private def listRegularFiles(file: File): Seq[File] = {
    if (file.isFile) Seq(file)
    else {
      val fList = listFiles(file)
      fList.map(f => listRegularFiles(f)).flatten
    }
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis
    try {
      val mainDir = {
        //val testDir = new File("""D:\tmp\julia\DUST1""")
        //val testDir = new File("""D:\tmp\julia\BEE1""")
        val testDir = new File("""D:\tmp\aqa\CBCT\MQATX2OBI2019Q3""")
        //val testDir = new File("""D:\tmp\julia\bad""")
        if (args.isEmpty && (testDir.isDirectory)) testDir
        else new File(args.head)
      }
      val outDir = new File(mainDir.getParentFile, mainDir.getName + "dag")
      Utility.deleteFileTree(outDir)
      outDir.mkdirs

      val allFiles = listRegularFiles(mainDir).filter(f => f.getName.toLowerCase.endsWith(".dcm"))
      Trace.trace("allFiles.size: " + allFiles.size)

      val uniqueDcm = {
        val dclFlList = scala.collection.mutable.HashMap[String, DcmFl]()
        allFiles.map(f => {
          val dcmFl = new DcmFl(f)
          val sop = dcmFl.sop
          if (!dclFlList.contains(sop)) dclFlList.put(sop, dcmFl)
        })
        val list = dclFlList.values.toList
        Trace.trace("Total number of unique files found: " + list.size)
        list
      }

      Trace.trace("Total number of unique files found: " + uniqueDcm.size)

      val frmOfRefList = uniqueDcm.map(d => d.frameOfRef).distinct
      Trace.trace("Number of frames of ref: " + frmOfRefList.size)

      frmOfRefList.zipWithIndex.map(frmOfRefIdx => fix(frmOfRefIdx._1, frmOfRefIdx._2 + 1, uniqueDcm, outDir))

      //saveOther(allDcm, outDir)

      Trace.trace(mainDir.getName + "  Elapsed ms: " + (System.currentTimeMillis - start))
    } catch {
      case t: Throwable => {
        Trace.trace("Elapsed ms: " + (System.currentTimeMillis - start) + "    Unexpected exception: " + fmtEx(t))
      }
    }
  }

}
