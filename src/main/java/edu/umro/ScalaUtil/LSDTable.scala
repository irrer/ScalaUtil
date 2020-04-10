package edu.umro.ScalaUtil

import java.io.File
import edu.umro.util.Utility
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.TransferSyntax
import java.util.Date
import com.pixelmed.dicom.OtherByteAttribute
import com.pixelmed.dicom.OtherWordAttribute
import java.io.FileOutputStream
import com.pixelmed.dicom.FileMetaInformation
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import com.pixelmed.dicom.Attribute
import java.text.SimpleDateFormat
import org.scalatest.Fact.IsEqvTo
import java.util.Calendar
import java.util.TimeZone
import com.pixelmed.dicom.FloatSingleAttribute
import com.pixelmed.dicom.FloatDoubleAttribute

//import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy

object LSDTable extends Logging {

  private val frameOfRefSet = scala.collection.mutable.Set[String]()

  //  val calendar = Calendar.getInstance
  //  val tzOffset_ms = calendar.get(Calendar.ZONE_OFFSET) + calendar.get(Calendar.DST_OFFSET)
  val dateFormat = new SimpleDateFormat("_yyyy-MM-dd_HH-mm-ss-SSS")

  private val future = new SimpleDateFormat("yyyyMMddHHMM").parse("210001011111")

  private var dcmFlCount = 0

  private def prnt(text: String) = println(text)

  def textDateOf(alList: Seq[DcmFl]): String = {
    try {
      val maxDate = new Date(alList.map(ct => ct.date.getTime).max)
      dateFormat.format(new Date(maxDate.getTime))
    } catch {
      case t: Throwable => {
        if (alList.nonEmpty) {
          Trace.trace("bad date for list of als")
        }
        dateFormat.format(new Date(future.getTime))
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

  //  private def modalityOf(al: AttributeList) = al.get(TagFromName.Modality).getSingleStringValueOrEmptyString
  //
  //  private def sopOf(al: AttributeList) = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
  //
  //  private def seriesUidOf(al: AttributeList) = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
  //
  //  private def seriesDescriptionOf(al: AttributeList) = al.get(TagFromName.SeriesDescription).getSingleStringValueOrEmptyString
  //
  //  private def studyDescriptionOf(al: AttributeList) = al.get(TagFromName.StudyDescription).getSingleStringValueOrEmptyString
  //
  //  private def manufModOf(al: AttributeList) = {
  //    val dflt = "unknown"
  //    try {
  //      al.get(TagFromName.ManufacturerModelName).getSingleStringValueOrDefault(dflt)
  //    } catch {
  //      case t: Throwable => dflt
  //    }
  //  }
  //
  //  private def frameOfRefOf(al: AttributeList): String = {
  //    val list = DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).map(at => at.getSingleStringValueOrEmptyString)
  //
  //    if (modalityOf(al).equals("REG") && (list.size > 1)) {
  //      val mainFrmOfRef = al.get(TagFromName.FrameOfReferenceUID).getSingleStringValueOrEmptyString
  //      list.filterNot(frmOfRef => frmOfRef.eq(mainFrmOfRef)).head
  //    } else {
  //      if (list.isEmpty)
  //        "none"
  //      else
  //        list.head
  //    }
  //  }

  private def refPlanOf(al: AttributeList): String = {
    if (al.get(TagFromName.ReferencedRTPlanSequence) != null) {
      val refSeq = DicomUtil.seqToAttr(al, TagFromName.ReferencedRTPlanSequence).head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
      refSeq
    } else ""
  }

  def dateOfAl(al: AttributeList): Date = {
    val dateTimeTagPairList = List(
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.CreationDate, TagFromName.CreationTime),
      (TagFromName.StudyDate, TagFromName.StudyTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    val list = dateTimeTagPairList.map(dt => DicomUtil.getTimeAndDate(al, dt._1, dt._2))
    list.flatten.head
  }

  //  val tagsOfInterest = Seq(
  //    TagFromName.AcquisitionDate,
  //    TagFromName.AcquisitionTime,
  //    TagFromName.ContentDate,
  //    TagFromName.ContentTime,
  //    TagFromName.CreationDate,
  //    TagFromName.CreationTime,
  //    TagFromName.ImagePositionPatient,
  //    TagFromName.SliceLocation,
  //    TagFromName.InstanceCreationDate,
  //    TagFromName.InstanceCreationTime,
  //    TagFromName.ManufacturerModelName,
  //    TagFromName.Modality,
  //    TagFromName.SeriesDate,
  //    TagFromName.SeriesInstanceUID,
  //    TagFromName.SeriesTime,
  //    TagFromName.SOPInstanceUID,
  //    TagFromName.SeriesDescription,
  //    TagFromName.StudyDescription,
  //    TagFromName.PatientID,
  //    TagFromName.PatientName,
  //    TagFromName.TableTopEccentricAngle,
  //    TagFromName.TableTopPitchAngle,
  //    TagFromName.TableTopRollAngle,
  //    TagFromName.StudyDate,
  //    TagFromName.StudyTime)
  //
  //  def tagToLong(t: AttributeTag): Long = {
  //    (t.getGroup.toLong << 16) + t.getElement
  //  }
  //
  //  val lastTagOfInterestAsLong = tagToLong(tagsOfInterest.maxBy(tagToLong))
  //
  //  private def readPartial(file: File, lastTagAsLong: Long = lastTagOfInterestAsLong): AttributeList = {
  //
  //    var savedAttrList = new AttributeList
  //    class ReadStrategy extends ReadTerminationStrategy {
  //      override def terminate(al: AttributeList, tag: AttributeTag, bytesRead: Long): Boolean = {
  //        savedAttrList = al
  //        tagToLong(tag) > lastTagAsLong
  //      }
  //    }
  //
  //    val readStrategy = new ReadStrategy
  //
  //    try {
  //      (new AttributeList).read(file, readStrategy)
  //    } catch {
  //      case t: Throwable => ;
  //    }
  //    savedAttrList
  //  }

  var timeToShowProgress = System.currentTimeMillis + 1000

  private def getAllTableAngles(al: AttributeList): Seq[Attribute] = {
    val tableAngleTagSet = Set(
      TagFromName.TableTopEccentricAngle,
      TagFromName.TableTopPitchAngle,
      TagFromName.TableTopRollAngle)
    DicomUtil.findAll(al, tableAngleTagSet)
  }

  /**
   * Set all table angles in the given attribute list to zero.
   */
  private def zeroTableAngles(al: AttributeList): Unit = {

    def zero(attr: Attribute) = {
      attr match {
        case flt: FloatSingleAttribute => {
          val size = flt.getFloatValues.size
          flt.removeValues
          (0 until size).map(i => flt.addValue(0.toFloat))
        }
        case dbl: FloatDoubleAttribute => {
          val size = dbl.getFloatValues.size
          dbl.removeValues
          (0 until size).map(i => dbl.addValue(0.toDouble))
        }
        case _ => {
          throw new Exception("Unknown table angle attribute type " + attr.getClass.getName + "  value: " + attr.toString.replace('\0', ' '))
        }
      }
    }

    getAllTableAngles(al).map(attr => zero(attr))
  }

  private class DcmFl(f: File) {
    prnt("Reading " + f.getAbsolutePath)

    val al = new AttributeList
    al.read(f)

    def get(tag: AttributeTag): String = {
      try {
        new String(al.get(tag).getSingleStringValueOrEmptyString)
      } catch {
        case t: Throwable => ""
      }
    }

    val file = f
    val Modality = get(TagFromName.Modality)
    val PatientID = get(TagFromName.PatientID)
    val PatientName = get(TagFromName.PatientName)
    val SOPInstanceUID = get(TagFromName.SOPInstanceUID)

    val SeriesInstanceUID = get(TagFromName.SeriesInstanceUID)
    val FrameOfReferenceUID = get(TagFromName.FrameOfReferenceUID)

    val SeriesDescription = get(TagFromName.SeriesDescription)
    val StudyDescription = get(TagFromName.StudyDescription)

    val ManufacturerModelName = get(TagFromName.ManufacturerModelName)
    val Manufacturer = get(TagFromName.Manufacturer)

    /**
     * Absolute value of largest table angle.
     */
    val maxTableAngle: Double = {
      def sToDbl(s: String): Double = {
        try {
          s.toDouble
        } catch {
          case t: Throwable => 0.0
        }
      }

      val angleList =
        getAllTableAngles(al).
          map(attr => attr.getStringValues).
          flatten.
          map(s => sToDbl(s)).
          map(angle => angle.abs)
      if (angleList.isEmpty) 0 else angleList.max
    }

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

    val dateText = dateFormat.format(new Date(date.getTime))

    def copyTo(dest: File) = {
      val data = FileUtil.readBinaryFile(file).right.get
      dest.getParentFile.mkdirs
      FileUtil.writeBinaryFile(dest, data)
    }

    dcmFlCount = dcmFlCount + 1
    val now = System.currentTimeMillis
    if (now > timeToShowProgress) {
      prnt("Files read: " + dcmFlCount)
      timeToShowProgress = now + 1000
      if ((dcmFlCount % 1000) == 0) {
        Runtime.getRuntime.gc
        Thread.sleep(50)
      }
    }

    override def toString = {
      PatientName + " : " + PatientID + "  " + Modality + "  " + dateText + "  " + file.getAbsolutePath
    }
  }

  private def makeDcmFl(file: File): Option[DcmFl] = {
    try {
      Some(new DcmFl(file))
    } catch {
      case t: Throwable => {
        prnt("Ignoring as a non-DICOM file: " + file.getAbsolutePath)
        None
      }
    }
  }

  def resolution(f: File): Int = {
    val al = readFile(f)
    al.get(TagFromName.Rows).getIntegerValues.head * al.get(TagFromName.Columns).getIntegerValues.head
  }

  private def fixRtstruct(rtstructDF: DcmFl, outDir: File, frameOfRefIndex: Int, rtstructIndex: Int, imageSeriesList: Seq[Seq[DcmFl]]) = {}

  def fmt(i: Int) = i.formatted("%03d")

  private def saveImageSeries(imageSeries: Seq[DcmFl], index: Int, outDir: File) = {
    val modality = imageSeries.head.Modality
    val manfName = imageSeries.head.ManufacturerModelName.replaceAll("[^a-zA-Z0-9]", "_")
    val imageDirName = (modality + "_" + manfName + "_" + textDateOf(imageSeries) + "_" + fmt(index)).replaceAll("___*", "_")
    val imageDir = new File(outDir, imageDirName)
    Trace.trace("Saving " + modality + " series " + imageDir.getAbsolutePath)
    imageDir.mkdirs
    imageSeries.zipWithIndex.map(imgIdx => {
      val dest = new File(imageDir, modality + "-" + fmt(imgIdx._2 + 1) + ".dcm")
      imgIdx._1.copyTo(dest)
    })
  }

  //  private def saveImageSeriesList(imageSeriesList: Seq[Seq[DcmFl]], index: Int, outDir: File) = {
  //    imageSeriesList.zipWithIndex.map(si => saveImageSeries(si._1, si._2 + 1, outDir))
  //  }
  //
  //  private def saveRtplan(rtplanDM: DcmFl, outDir: File, frmOfRefIndex: Int, planIndex: Int) = {
  //    val dest = new File(outDir, "RTPLAN_" + rtplanDM.dateText + "_" + fmt(planIndex) + ".dcm")
  //    Trace.trace("Saving RTPLAN " + dest.getAbsolutePath)
  //    rtplanDM.copyTo(dest)
  //  }
  //
  //  private def saveRtimage(rtimageDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtimageIndex: Int) = {
  //    val dest = new File(outDir, "RTIMAGE" + rtimageDM.dateText + "_" + fmt(rtimageIndex) + ".dcm")
  //    Trace.trace("Saving RTIMAGE " + dest.getAbsolutePath)
  //    rtimageDM.copyTo(dest)
  //  }
  //
  //  private def saveReg(regDM: DcmFl, outDir: File, frmOfRefIndex: Int, regIndex: Int) = {
  //    val dest = new File(outDir, "REG" + regDM.dateText + "_" + fmt(regIndex) + ".dcm")
  //    Trace.trace("Saving REG " + dest.getAbsolutePath)
  //    regDM.copyTo(dest)
  //  }
  //
  //  private def saveRtdose(rtdoseDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtdoseIndex: Int) = {
  //    val dest = new File(outDir, "RTDOSE" + rtdoseDM.dateText + "_" + fmt(rtdoseIndex) + ".dcm")
  //    Trace.trace("Saving RTDOSE " + dest.getAbsolutePath)
  //    rtdoseDM.copyTo(dest)
  //  }
  //
  //  private def saveRtrecord(regDM: DcmFl, outDir: File, frmOfRefIndex: Int, rtrecordIndex: Int) = {
  //    val dest = new File(outDir, "RTRECORD_" + regDM.dateText + "_" + fmt(rtrecordIndex) + ".dcm")
  //    Trace.trace("Saving RTRECORD " + dest.getAbsolutePath)
  //    regDM.copyTo(dest)
  //  }
  //
  //  private def getByRefPlan(frmOfRef: String, rtplanList: Seq[DcmFl], allDcm: Seq[DcmFl]): Seq[DcmFl] = {
  //    val rtplanSopList = rtplanList.map(rtplan => rtplan.SOPInstanceUID)
  //  }
  //
  //  private def fix(frmOfRef: String, frmOfRefIndex: Int, allDcm: Seq[DcmFl], outDir: File) = {
  //    Trace.trace("Processing frame of ref: " + frmOfRefIndex)
  //    val frmOfRefList = allDcm.filter(d => d.frameOfRef.equals(frmOfRef))
  //    val imageSeriesList = frmOfRefList.filter(d => d.modality.equals("CT") || d.modality.equals("MR")).
  //      groupBy(_.seriesUid).
  //      map(ss => ss._2.sortBy(d => d.position)).
  //      toSeq.
  //      sortBy(s => resolution(s.head.file))
  //
  //    val frmOfRefDirDate = {
  //      (imageSeriesList.nonEmpty, frmOfRefList.find(f => f.modality.equals("RTIMAGE"))) match {
  //        case (true, _) => textDateOf(imageSeriesList.head)
  //        case (_, Some(dcmFl)) => textDateOf(frmOfRefList.filter(d => d.modality.equals("RTIMAGE")))
  //        case _ => textDateOf(frmOfRefList)
  //      }
  //    }
  //
  //    val frmOfRefDir = new File(outDir, "FrmofRef" + frmOfRefDirDate + "_" + fmt(frmOfRefIndex))
  //    if (imageSeriesList.nonEmpty) saveImageSeriesList(imageSeriesList, frmOfRefIndex, frmOfRefDir)
  //
  //    val rtstructList = frmOfRefList.filter(d => d.modality.equals("RTSTRUCT"))
  //    // private def fixRtstruct(rtstructDF: DcmFl, outDir : File, index: Int, frameOfRefIndex: Int, rtstructIndex: Int, ctList: Seq[DcmFl]) = {
  //    rtstructList.zipWithIndex.map(di => fixRtstruct(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1, imageSeriesList))
  //
  //    val rtplanList = allDcm.filter(d => d.frameOfRef.equals(frmOfRef) && d.modality.equals("RTPLAN"))
  //    // private def fixRtstruct(rtstructDF: DcmFl, outDir : File, index: Int, frameOfRefIndex: Int, rtstructIndex: Int, ctList: Seq[DcmFl]) = {
  //    rtplanList.zipWithIndex.map(di => saveRtplan(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))
  //
  //    val rtimageList = frmOfRefList.filter(d => d.modality.equals("RTIMAGE"))
  //    rtimageList.zipWithIndex.map(di => saveRtimage(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))
  //
  //    val regList = frmOfRefList.filter(d => d.modality.equals("REG"))
  //    regList.zipWithIndex.map(di => saveReg(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))
  //
  //    val refRtplanList = getByRefPlan(frmOfRef, rtplanList, allDcm).filter(d => d.modality.equals("RTRECORD") || d.modality.equals("RTDOSE"))
  //
  //    val rtdoseList = refRtplanList.filter(d => d.modality.equals("RTDOSE"))
  //    rtdoseList.zipWithIndex.map(di => saveRtdose(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))
  //
  //    val rtrecordList = refRtplanList.filter(d => d.modality.equals("RTRECORD"))
  //    rtrecordList.zipWithIndex.map(di => saveRtrecord(di._1, frmOfRefDir, frmOfRefIndex, (di._2) + 1))
  //  }
  //
  //  private def saveOther(allDcm: Seq[DcmFl], outDir: File) = {
  //    val noFrameOfRef = allDcm.filter(d => d.frameOfRef.size <= 5)
  //    val byModality = allDcm.filter(d =>
  //      (!d.modality.equals("CT")) &&
  //        (!d.modality.equals("MR")) &&
  //        (!d.modality.equals("RTSTRUCT")) &&
  //        (!d.modality.equals("RTIMAGE")) &&
  //        (!d.modality.equals("REG")) &&
  //        (!d.modality.equals("RTRECORD")) &&
  //        (!d.modality.equals("RTDOSE")) &&
  //        (!d.modality.equals("RTPLAN")))
  //
  //    def saveSeries(series: Seq[DcmFl], seriesIndex: Int) = {
  //      val seriesDate = textDateOf(series)
  //      if (series.size == 1) {
  //        val dest = new File(outDir, series.head.modality + seriesDate + "_" + fmt(seriesIndex) + ".dcm")
  //        series.head.copyTo(dest)
  //      } else {
  //        val seriesDir = new File(outDir, series.head.modality + seriesDate + "_" + fmt(seriesIndex))
  //        seriesDir.mkdirs
  //        series.zipWithIndex.map(di => {
  //          val dest = new File(seriesDir, series.head.modality + seriesDate + "_" + fmt(di._2 + 1) + ".dcm")
  //          di._1.copyTo(dest)
  //        })
  //      }
  //    }
  //
  //    (noFrameOfRef ++ byModality).
  //      groupBy(_.seriesUid).
  //      map(ss => ss._2).
  //      zipWithIndex.map(ssi => saveSeries(ssi._1, ssi._2 + 1))
  //  }

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

  /**
   * Recursively descend a directory tree, acquiring a list of all files.
   */
  private def listRegularFiles(file: File): Seq[File] = {
    if (file.isFile) Seq(file)
    else {
      val fList = listFiles(file)
      fList.map(f => listRegularFiles(f)).flatten
    }
  }

  private def allSameSamePatient(allDcmFl: Seq[DcmFl]) = {
    val patList = allDcmFl.groupBy(df => df.PatientID)
    if (patList.size > 1) {
      val patDesc = patList.map(fr => fr._2.head.toString).mkString("\n    ")
      prnt("There are " + patList.size + " patients referenced, but there should be only one.  The differing files are:\n    " + patDesc)
      false
    } else true
  }

  private def allSameFrameOfReference(allDcmFl: Seq[DcmFl]): Boolean = {
    val forList = allDcmFl.groupBy(df => df.FrameOfReferenceUID)
    if (forList.size > 1) {
      val forDesc = forList.map(fr => fr._2.head.toString + " --> " + fr._2.head.FrameOfReferenceUID).mkString("\n    ")
      prnt("There are " + forList.size + " frames of reference, but there should be only one.  Differing files are:\n    " + forDesc)
      false
    } else true
  }

  /**
   * Make sure all table angles are acceptably close to zero.
   */
  private def allTableAnglesNearZero(allDcmFl: Seq[DcmFl]): Boolean = {
    val envName = "MaxTableAngle"
    val defaultTableAngle = 1.0
    val text = System.getenv(envName)

    val MaxTableAngle: Double = if (text == null) {
      prnt("no " + envName + " environment variable specified.  Using default of " + defaultTableAngle)
      defaultTableAngle
    } else {
      try {
        text.toDouble
      } catch {
        case t: Throwable =>
          {
            prnt("table angle specified by " + envName + " is " + text + " which is not a valid floating point value.  Using default of " + defaultTableAngle)
            defaultTableAngle
          }
      }
    }

    prnt("Using " + envName + " of " + MaxTableAngle)

    val tooBig = allDcmFl.filter(df => df.maxTableAngle > MaxTableAngle)
    if (tooBig.nonEmpty) {
      prnt("The following files have table angles whose absolute values are larger than " + MaxTableAngle + ":\n    ")
      tooBig.mkString("\n    ")
    }

    tooBig.isEmpty
  }

  /**
   * Use the
   */
  private def establishOutDir(args: Array[String], dcmFl: DcmFl) = {
    ???
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis
    try {

      val allFiles = args.map(a => listRegularFiles(new File(a))).flatten
      prnt("number of files found: " + allFiles.size)

      val allDcmFl = allFiles.map(f => makeDcmFl(f)).flatten
      prnt("Number of DICOM files found: " + allDcmFl.size)

      if ((allSameSamePatient(allDcmFl) && allSameFrameOfReference(allDcmFl) && allTableAnglesNearZero(allDcmFl)) || true) {
        Trace.trace //  val outDir = establishOutDir(args, allDcmFl.head)

        val outDir = new File(allFiles.head.getParent, "output")
        outDir.delete
        outDir.mkdirs
        prnt("Output directory: " + outDir.getAbsolutePath)
      }

      //saveOther(allDcm, outDir)

      prnt("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
    } catch {
      case t: Throwable => {
        Trace.trace("Elapsed ms: " + (System.currentTimeMillis - start) + "    Unexpected exception: " + fmtEx(t))
      }
    }
  }

}
