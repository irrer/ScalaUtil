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

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeTagAttribute
import com.pixelmed.dicom.DicomFileUtilities
import com.pixelmed.dicom.DicomInputStream
import com.pixelmed.dicom.DicomOutputStream
import com.pixelmed.dicom.FileMetaInformation
import com.pixelmed.dicom.OtherByteAttribute
import com.pixelmed.dicom.OtherFloatAttribute
import com.pixelmed.dicom.OtherWordAttribute
import com.pixelmed.dicom.SOPClass
import com.pixelmed.dicom.SOPClassDescriptions
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.TagByName
import resource.managed

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStream
import java.text.SimpleDateFormat
import java.util
import java.util.Date
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

object DicomUtil {

  val dictionary = TagByName.dict

  private val minorIndent = "  "
  private val indentText = minorIndent + minorIndent

  /** DICOM compatible date format. */
  val dicomDateFormat = new SimpleDateFormat("yyyyMMdd")

  /** DICOM compatible time format. */
  //noinspection SpellCheckingInspection
  val dicomTimeFormat = new SimpleDateFormat("HHmmss.SSS")

  //noinspection SpellCheckingInspection
  val dicomTimeFormatSimple = new SimpleDateFormat("HHmmss")

  /**
    * Parse a text string in DICOM time format and return ms.  On failure to parse return None.
    */
  def parseDicomTime(text: String): Option[Long] = {
    val parts = text.split('.')

    try {
      val upper = Util.parseDate(dicomTimeFormatSimple, parts(0))
      val ms: Long = if (parts.length > 1) {
        val uS = (parts(1) + "000000").take(6).toDouble
        (uS / 1000).round
      } else 0

      Some(upper.getTime + ms)
    } catch {
      case _: Throwable => None
    }
  }

  /** Used for converting a DICOM date+time pair into a <code>Date</code> */
  //noinspection SpellCheckingInspection
  private val dicomDateTimeFormat = new SimpleDateFormat("yyyyMMddHHmmss.SSS")

  /**
    * Convert date and time pair into java.util.Date.  Note that time will only be accurate to the ms.  Note that
    * the date and time must be done together so as to get the time zone and daylight savings time right.
    */
  def getTimeAndDate(al: AttributeList, dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
    try {
      val dateText = al.get(dateTag).getSingleStringValueOrNull
      val timeText = {
        val t = al.get(timeTag).getSingleStringValueOrNull
        val t2 =
          if (t.contains('.')) t + "000"
          else t + ".000"
        t2.take(10)
      }
      Some(Util.parseDate(dicomDateTimeFormat, dateText + timeText))
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * List of value representations that can be displayed as strings in the
    * text version of the preview.
    */
  private val TEXTUAL_VR = List(
    ValueRepresentation.AE,
    ValueRepresentation.AS,
    ValueRepresentation.CS,
    ValueRepresentation.DA,
    ValueRepresentation.DS,
    ValueRepresentation.DT,
    ValueRepresentation.FL,
    ValueRepresentation.FD,
    ValueRepresentation.IS,
    ValueRepresentation.LO,
    ValueRepresentation.LT,
    ValueRepresentation.PN,
    ValueRepresentation.SH,
    ValueRepresentation.SL,
    ValueRepresentation.SS,
    ValueRepresentation.ST,
    ValueRepresentation.TM,
    ValueRepresentation.UI,
    ValueRepresentation.UL,
    ValueRepresentation.US,
    ValueRepresentation.UT,
    ValueRepresentation.XS,
    ValueRepresentation.XO
  )

  /** A quickly searchable list of value representations. */
  private val vrSet = new util.HashSet[String]

  TEXTUAL_VR.map(vr => vrSet.add(new String(vr)))

  /**
    * Format an attribute tag as a string.
    */
  def formatAttrTag(tag: AttributeTag): String = tag.getGroup.formatted("%04x") + "," + tag.getElement.formatted("%04x")

  /**
    * Convert a single non-sequence attribute to a human readable text format.
    *
    * @param attribute
    * Attribute to format.
    * @return String version of attribute.
    */
  def attributeToString(attribute: Attribute, indentLevel: String): String = {
    val tag = attribute.getTag
    val vrDict = dictionary.getValueRepresentationFromTag(tag)
    val vr = if (vrDict == null) attribute.getVR else vrDict
    val VALUE_SEPARATOR = " \\ "
    val MAX_LINE_LENGTH = 500
    val MAX_REPEAT = 3 // if a value repeats more than this many times, then print a compressed version

    def foldStringList(list: List[String], valueSeparator: String = VALUE_SEPARATOR): String = {

      case class Group(text: String, size: Int) {
        override def toString: String = {
          if (size <= MAX_REPEAT) // if there are only a few of them, then print them all
            (0 until size).map(_ => text).mkString(VALUE_SEPARATOR)
          else
            text + " (repeated " + size + " times)" // if there are many, then compress them
        }
      }

      def makeGroups(seq: Seq[String], groupList: Seq[Group] = Seq()): Seq[Group] = {
        0 match {
          case _ if seq.isEmpty =>
            groupList
          case _ if groupList.isEmpty =>
            makeGroups(seq.tail, Seq(Group(seq.head, 1)))
          case _ =>
            val g = groupList.last
            if (g.text.equals(seq.head))
              makeGroups(seq.tail, groupList.dropRight(1) :+ Group(g.text, g.size + 1))
            else {
              if (groupList.size > (MAX_LINE_LENGTH / 5)) // if the line is getting too long, then stop.
                groupList
              else
                makeGroups(seq.tail, groupList :+ Group(seq.head, 1))
            }
        }
      }

      val text = makeGroups(list).mkString(valueSeparator)
      text
    }

    def tagDetails: String = {
      val vrText: String = if (vr == null) "??" else new String(vr)
      formatAttrTag(tag) + " " + vrText
    }

    def toTextualVR: String = {
      val classSop: String = {
        val value = attribute.getSingleStringValueOrNull
        if ((value != null) && ValueRepresentation.isUniqueIdentifierVR(vr) && SOPClassDescriptions.getDescriptionFromUID(value).nonEmpty) {
          " (" + SOPClassDescriptions.getDescriptionFromUID(value) + ")"
        } else ""
      }
      val text = foldStringList(
        if (attribute.getStringValues == null) List("<null>")
        else attribute.getStringValues.toList,
        VALUE_SEPARATOR
      )
      text + classSop
    }

    def toAttributeTagVR(attr: AttributeTagAttribute): String = {
      val textList = attr.getAttributeTagValues.map(t => {
        if (dictionary.getNameFromTag(t) == null) ":<unknown>"
        else ":" + dictionary.getNameFromTag(t)
      })

      foldStringList(textList.toList, VALUE_SEPARATOR)
    }

    def limitedCopy[A](raw: Array[A], max: Int): List[A] = {
      if (raw == null)
        List[A]()
      else
        raw.take(max).toList
    }

    def toOtherByte(attr: OtherByteAttribute): String = {
      val data = limitedCopy(attr.getByteValues, MAX_LINE_LENGTH / 3)
      data.map(d => (d.toInt & 0xff).formatted("0x%x")).mkString(" ")
    }

    def toOtherFloat(attr: OtherFloatAttribute): String = {
      val data = limitedCopy(attr.getFloatValues, MAX_LINE_LENGTH / 3)
      foldStringList(data.map(d => d.toString), " ")
    }

    def toOtherWord(attr: OtherWordAttribute): String = {
      val data = limitedCopy(attr.getShortValues, MAX_LINE_LENGTH / 3)
      data.map(d => ((d & 0xffff) / 256).formatted("0x%x")).mkString(" ")
    }

    def toSequenceAttribute(attr: SequenceAttribute): String = {
      val size = attr.getNumberOfItems
      val textList = (0 until size).map(i => {
        (if (i == 0) "\n" else "") +
          indentLevel + minorIndent + "Item " + (i + 1) + " / " + size +
          "\n" + attributeListToString(attr.getItem(i).getAttributeList, indentLevel + indentText)
      })
      val text = textList.foldLeft("")((t, a) => t + a)
      if (text.endsWith("\n")) text.subSequence(0, text.length - 1).toString else text
    }

    val valueText: String =
      if (attribute.isInstanceOf[SequenceAttribute]) {
        toSequenceAttribute(attribute.asInstanceOf[SequenceAttribute])
      } else {
        attribute match {
          case _ if (vr != null) && vrSet.contains(new String(vr)) => toTextualVR
          case attr: AttributeTagAttribute                         => toAttributeTagVR(attr)
          case attr: OtherByteAttribute                            => toOtherByte(attr)
          case attr: OtherFloatAttribute                           => toOtherFloat(attr)
          case attr: OtherWordAttribute                            => toOtherWord(attr)
          case attr: SequenceAttribute                             => toSequenceAttribute(attr)
          case _                                                   => "unknown"
        }
      }.replace('\n', ' ').replace('\u0000', ' ').replace('\r', ' ') // remove funky characters
    //}.replace('\n', ' ').replace('\u0000', ' ').replace('\r', ' ') // remove funky characters

    val tagName = if (dictionary.getNameFromTag(tag) == null) "<unknown>" else dictionary.getNameFromTag(tag)

    indentLevel + tagDetails + "  " + tagName + ": " + valueText
  }

  private def attributeListToString(attributeList: AttributeList, indent: String): String = {
    attributeList.keySet.toArray.toList
      .map(tag => {
        val t = tag
        val a = attributeList.get(t)
        attributeToString(a, indent)
      })
      .foldLeft("")((t, a) => t + a + "\n")
  }

  def attributeListToString(attributeList: AttributeList): String = attributeListToString(attributeList, "")

  /**
    * Represent the components of a person name (Value Representation PN) in DICOM format.
    *
    * Smith^John^Q --> John Q Smith
    */
  case class DicomPersonName(familyNameComplex: Option[String], givenNameComplex: Option[String], middleName: Option[String], namePrefix: Option[String], nameSuffix: Option[String]) {
    def partToStr(part: Option[String]): String =
      if (part.isDefined) {
        part.get + " "
      } else
        ""

    override def toString: String = (partToStr(namePrefix) + partToStr(givenNameComplex) + partToStr(middleName) + partToStr(familyNameComplex) + partToStr(nameSuffix)).replaceAll("  *", " ").trim
  }

  /**
    * Given a person name (Value Representation PN) in DICOM format, break it down into its components.
    */
  def parseDicomPersonName(text: String): DicomPersonName = {
    val pn = text.split("\\^")

    def getPn(i: Int): Option[String] = if (pn.size > i) Some(pn(i)) else None

    DicomPersonName(getPn(0), getPn(1), getPn(2), getPn(3), getPn(4))
  }

  /**
    * Compare two DICOM files for the purpose of sorting them in the order that humans expect.
    */
  def compareDicom(a: AttributeList, b: AttributeList): Int = {

    def nullAttrCheck(tag: AttributeTag): Either[Int, (Attribute, Attribute)] = {

      (a.get(tag), b.get(tag)) match {
        case (null, null)   => Left(0)
        case (null, _)      => Left(-1)
        case (_, null)      => Left(1)
        case (aAttr, bAttr) => Right(aAttr, bAttr)
      }
    }

    def compareString(tag: AttributeTag)(dummy: Any): Int = {

      def compareVal(aAttr: Attribute, bAttr: Attribute): Int = {
        (aAttr.getSingleStringValueOrNull, bAttr.getSingleStringValueOrNull) match {
          case (null, null) => 0
          case (null, _)    => -1
          case (_, null)    => 1
          case (aVal, bVal) => aVal.compareTo(bVal)
        }
      }

      nullAttrCheck(tag) match {
        case Left(c)               => c
        case Right((aAttr, bAttr)) => compareVal(aAttr, bAttr)
      }

    }

    def compareDouble(tag: AttributeTag, index: Int)(dummy: Any): Int = {

      def comprVal(aAttr: Attribute, bAttr: Attribute): Int = {
        (aAttr.getDoubleValues, bAttr.getDoubleValues) match {
          case (null, null)                    => 0
          case (null, _)                       => -1
          case (_, null)                       => 1
          case (aVal, _) if aVal.size <= index => -1
          case (_, bVal) if bVal.size <= index => 1
          case (aVal, bVal)                    => aVal(index).compareTo(bVal(index))
        }
      }

      nullAttrCheck(tag) match {
        case Left(c)               => c
        case Right((aAttr, bAttr)) => comprVal(aAttr, bAttr)
      }

    }

    lazy val seq: Seq[Any => Int] = Seq(
      compareDouble(TagFromName.SliceLocation, 0),
      compareDouble(TagFromName.ImagePositionPatient, 0),
      compareDouble(TagFromName.ImagePositionPatient, 1),
      compareDouble(TagFromName.ImagePositionPatient, 2),
      compareDouble(TagFromName.InstanceNumber, 0),
      compareString(TagFromName.InstanceCreationDate),
      compareString(TagFromName.InstanceCreationTime),
      compareString(TagFromName.AcquisitionDate),
      compareString(TagFromName.AcquisitionTime),
      compareString(TagFromName.ContentDate),
      compareString(TagFromName.ContentTime),
      compareString(TagFromName.RTPlanDate),
      compareString(TagFromName.RTPlanTime),
      compareString(TagFromName.StructureSetDate),
      compareString(TagFromName.StructureSetTime),
      compareString(TagFromName.SOPInstanceUID)
    )

    val result = seq.view.map(func => func(0)).find(v => v != 0)

    if (result.isDefined) result.get else 0
  }

  /**
    * Sort a list of DICOM attribute lists.
    */
  def sortDicom(attributeListList: Seq[AttributeList]): Seq[AttributeList] = attributeListList.sortWith((a, b) => compareDicom(a, b) <= 0)

  /**
    * Return true if the DICOM is an image modality.
    */
  def isImageStorage(attributeList: AttributeList): Boolean = {
    SOPClass.isImageStorage(Attribute.getSingleStringValueOrEmptyString(attributeList, TagFromName.SOPClassUID))
  }

  /**
    * Make a new copy of an attribute list, not sharing any data with the original.
    *
    * @param source * List to copy.
    * @return Copy of list.
    *
    */
  def clone(source: AttributeList): AttributeList = {
    val dest = new AttributeList

    val transferSyntax = {
      val ts = source.get(TagFromName.TransferSyntaxUID)
      if ((ts != null) && ts.getStringValues.nonEmpty)
        ts.getStringValues.head
      else
        TransferSyntax.ExplicitVRLittleEndian; // DEFAULT_TRANSFER_SYNTAX;
    }
    val byteArrayOutputStream = new ByteArrayOutputStream
    val dicomOutputStream = new DicomOutputStream(byteArrayOutputStream, transferSyntax, transferSyntax)
    source.write(dicomOutputStream)

    val byteArrayInputStream = new ByteArrayInputStream(byteArrayOutputStream.toByteArray)
    dest.read(new DicomInputStream(byteArrayInputStream))

    dest
  }

  /**
    * Get the attribute lists of a sequence attribute.
    */
  def seqToAttr(al: AttributeList, tag: AttributeTag): Seq[AttributeList] = {
    val seq = al.get(tag).asInstanceOf[SequenceAttribute]
    (0 until seq.getNumberOfItems).map(i => seq.getItem(i).getAttributeList)
  }

  /**
    * Get the attribute lists of a sequence attribute.
    */
  def alOfSeq(al: SequenceAttribute): Seq[AttributeList] = {
    (0 until al.getNumberOfItems).map(i => al.getItem(i).getAttributeList)
  }

  /**
    * Get all instances of attributes that the caller deems interesting.
    *
    * @param attributeList to be searched.
    * @param interesting Returns true if the attribute is interesting.
    * @return List of interesting attributes.
    */
  def findAll(attributeList: AttributeList, interesting: Attribute => Boolean): IndexedSeq[Attribute] = {

    def childSeq(al: AttributeList): IndexedSeq[AttributeList] = {
      val seqList = al.values.toArray.filter(at => at.isInstanceOf[SequenceAttribute]).map(at => at.asInstanceOf[SequenceAttribute])
      val alListList = seqList.flatMap(seq => (0 until seq.getNumberOfItems).map(i => seq.getItem(i).getAttributeList))
      alListList.toIndexedSeq
    }

    val atList = attributeList.values.toArray.toList.toIndexedSeq.map(at => at.asInstanceOf[Attribute])
    val listOfInterest = atList.filter(interesting)
    val all = listOfInterest ++ childSeq(attributeList).flatMap(child => findAll(child, interesting))
    all
  }

  /**
    * Get all instances of attributes with a tag on the given list by searching the given <code>AttributeList</code> recursively.
    */
  def findAll(attributeList: AttributeList, tagSet: Set[AttributeTag]): IndexedSeq[Attribute] = {
    findAll(attributeList, attr => tagSet.contains(attr.getTag))
  }

  def findAllSingle(attributeList: AttributeList, tag: AttributeTag): IndexedSeq[Attribute] = findAll(attributeList, Set(tag))

  private def getTransferSyntax(attributeList: AttributeList): String = {
    val ts = attributeList.get(TagFromName.TransferSyntaxUID)
    if ((ts != null) && (ts.getSingleStringValueOrNull != null)) ts.getSingleStringValueOrNull
    else TransferSyntax.ImplicitVRLittleEndian
  }

  /**
    * Write and attribute list to an output stream, preserving the TransferSyntaxUID if specified.  Flush and
    * close the output stream.  Throw an exception if there is an IO error.
    */
  def writeAttributeList(attributeList: AttributeList, outputStream: OutputStream, sourceApplication: String): Unit = {
    val transferSyntax = getTransferSyntax(attributeList)
    FileMetaInformation.addFileMetaInformation(attributeList, transferSyntax, sourceApplication)
    attributeList.write(outputStream, transferSyntax, true, true)
    outputStream.flush()
    outputStream.close()
  }

  /**
    * Write an attribute list to file, preserving the TransferSyntaxUID if specified.  If the
    * file exists, then delete it before writing. Create a new file before writing.  Flush and
    * close the file.  Throw an exception if there is an IO error.
    */
  def writeAttributeListToFile(attributeList: AttributeList, file: File, sourceApplication: String): Unit = {
    file.delete
    file.createNewFile
    writeAttributeList(attributeList, new FileOutputStream(file), sourceApplication)
  }

  /**
    * Write a list of attribute lists to a zipped byte array.  Each member will
    * have "[SOP_UID].dcm" as its entry name.
    */
  def dicomToZippedByteArray(alList: Seq[AttributeList]): Array[Byte] = {

    /**
      * Special write of attribute list that does not close the stream.
      */
    def writeAl(attributeList: AttributeList, outputStream: OutputStream): Unit = {
      val transferSyntax = getTransferSyntax(attributeList)
      val dout = new DicomOutputStream(outputStream, TransferSyntax.ExplicitVRLittleEndian, transferSyntax)
      attributeList.write(dout, true)
    }

    def addOneAlToZip(al: AttributeList, zipOut: ZipOutputStream): Unit = {
      val entryName = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString + ".dcm"
      val zipEntry = new ZipEntry(entryName)
      zipOut.putNextEntry(zipEntry)
      writeAl(al, zipOut)
    }

    val byteArrayOutputStream = new ByteArrayOutputStream
    managed(new ZipOutputStream(byteArrayOutputStream)) acquireAndGet { zipOut =>
      alList.map(al => addOneAlToZip(al, zipOut))
    }
    byteArrayOutputStream.toByteArray
  }

  /**
    * Write a list of named attribute lists to a zipped byte array.
    *
    * @param alListWithNames   : List of attribute+name pairs
    * @param sourceApplication : Source application in DICOM header
    *
    */
  def namedDicomToZippedByteArray(alListWithNames: Seq[(AttributeList, String)], sourceApplication: String): Array[Byte] = {

    /**
      * Special write of attribute list that does not close the stream.
      */
    def writeAl(attributeList: AttributeList, outputStream: OutputStream): Unit = {
      val transferSyntax = getTransferSyntax(attributeList)
      FileMetaInformation.addFileMetaInformation(attributeList, transferSyntax, sourceApplication)
      val dout = new DicomOutputStream(outputStream, TransferSyntax.ExplicitVRLittleEndian, transferSyntax)
      attributeList.write(dout, true)
    }

    def addOneAlToZip(al: AttributeList, name: String, zipOut: ZipOutputStream): Unit = {
      val zipEntry = new ZipEntry(name)
      zipOut.putNextEntry(zipEntry)
      writeAl(al, zipOut)
    }

    val byteArrayOutputStream = new ByteArrayOutputStream
    managed(new ZipOutputStream(byteArrayOutputStream)) acquireAndGet { zipOut =>
      alListWithNames.map(alName => addOneAlToZip(alName._1, alName._2, zipOut))
    }
    byteArrayOutputStream.toByteArray
  }

  /**
    * Given a byte array, attempt to convert it to an <code>AttributeList</code>.
    *
    * @param data Bytes representing a single DICOM object.
    * @return DICOM, or None if the content doesn't not represent a single DICOM object.
    */
  def byteArrayToDicom(data: Array[Byte]): Option[AttributeList] = {
    import scala.util.Success
    import scala.util.Try

    if (IsDicom.isDicomOrAcrNema(data)) {
      val result = {
        Try {
          val al = new AttributeList
          val dis = new DicomInputStream(new ByteArrayInputStream(data))
          al.read(dis)
          al
        } match {
          // Note that the check for dicom.size > 1 should not be necessary, but I think somehow Try is messing up the result
          case Success(dicom) if dicom.size > 1 => Some(dicom)
          case _                                => None
        }
      }
      result
    } else
      None
  }

  /**
    * Read a list of attribute lists from a zipped byte array.  Ignore non-DICOM content.
    */
  def zippedByteArrayToDicom(data: Array[Byte]): Seq[AttributeList] = {
    val contentList = FileUtil.writeZipToNamedByteArrays(new ByteArrayInputStream(data)).map(_._2)
    val dicomList = contentList.map(c => byteArrayToDicom(c)).filter(d => d.isDefined).map(d => d.get)
    dicomList
  }

  /**
    * Remove members of the sequence that match according to the given function.
    *
    * @param al                 : Contains the main SequenceAttribute
    * @param seqAttrTag         : Tag of main SequenceAttribute
    * @param identifyForRemoval : Returns true for each attribute list that should be removed.
    */
  def removeSeq(al: AttributeList, seqAttrTag: AttributeTag, identifyForRemoval: AttributeList => Boolean): Seq[AttributeList] = {
    val listPair = DicomUtil.seqToAttr(al, seqAttrTag).partition(identifyForRemoval)
    val remove = listPair._1
    val keep = listPair._2
    al.remove(seqAttrTag)
    val newSeq = new SequenceAttribute(seqAttrTag)
    keep.foreach(k => newSeq.addItem(k))
    al.put(newSeq)
    remove
  }

  /**
    * Detect different types (models) of treatment machines.
    */
  object TreatmentMachineType extends Enumeration {
    val Truebeam, Halcyon, ClinacC = Value

    private val truebeamNameList = Seq("TDS")
    private val halcyonNameList = Seq("RDS")
    private val clinacCNameList = Seq("2300IX")

    /**
      * Given an RTPLAN attribute list, return the type of treatment machine.
      *
      * Note that the ManufacturerModelName at the top level can often be incorrect, so it is better to
      * use the values from the BeamSequence.
      */
    def attrListToTreatmentMachineType(rtplan: AttributeList): Option[TreatmentMachineType.Value] = {
      val mainMMN = rtplan.get(TagFromName.ManufacturerModelName).getSingleStringValueOrEmptyString.trim.toUpperCase

      // get a list of all referenced models
      val ManufacturerModelNameList =
        findAllSingle(rtplan, TagFromName.ManufacturerModelName)
          .map(a => a.getSingleStringValueOrEmptyString.toUpperCase.trim)
          .distinct
          .filterNot(tmt => tmt.equals(""))
          .filterNot(tmt => tmt.equals(mainMMN))

      val tmt = ManufacturerModelNameList match {
        case _ if ManufacturerModelNameList.isEmpty                         => None
        case _ if truebeamNameList.contains(ManufacturerModelNameList.head) => Some(Truebeam)
        case _ if halcyonNameList.contains(ManufacturerModelNameList.head)  => Some(Halcyon)
        case _ if clinacCNameList.contains(ManufacturerModelNameList.head)  => Some(ClinacC)
        case _                                                              => None
      }
      tmt
    }
  }

  def isHalcyon(rtplan: AttributeList): Boolean = {
    val treatmentMachineType = TreatmentMachineType.attrListToTreatmentMachineType(rtplan)
    val isHalcy = treatmentMachineType.isDefined && treatmentMachineType.get.toString.equals(TreatmentMachineType.Halcyon.toString)
    isHalcy
  }

  /**
    * Given an RTPLAN and an RTIMAGE, get the beam's attribute list in the RTPLAN.
    *
    * @param plan       RTPLAN
    * @param BeamNumber beam number
    * @return Beam parameters.
    */
  def getBeamOfRtimage(plan: AttributeList, BeamNumber: Int): Option[AttributeList] = {
    try {
      val beam = DicomUtil.seqToAttr(plan, TagByName.BeamSequence).find(bs => bs.get(TagByName.BeamNumber).getIntegerValues.head == BeamNumber)
      beam
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Given an RTPLAN and an RTIMAGE, get the beam's attribute list in the RTPLAN.
    *
    * @param plan    RTPLAN
    * @param rtimage RTIMAGE
    * @return Beam parameters.
    */
  def getBeamOfRtimage(plan: AttributeList, rtimage: AttributeList): Option[AttributeList] = {
    try {
      val BeamNumber = rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head
      val beam = getBeamOfRtimage(plan, BeamNumber)
      beam
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Given an RTPLAN and an RTIMAGE, get the name of the beam that the RTIMAGE is referencing in the plan.
    *
    * @param plan    RTPLAN
    * @param rtimage RTIMAGE
    * @return Beam name.
    */
  def getBeamNameOfRtimage(plan: AttributeList, rtimage: AttributeList): Option[String] = {
    try {
      val al = getBeamOfRtimage(plan, rtimage).get
      Some(al.get(TagByName.BeamName).getSingleStringValueOrEmptyString)
    } catch {
      case _: Throwable => None
    }
  }

  /**
    * Self test.
    */
  def main(args: Array[String]): Unit = {

    if (true) {
      val file = new File("""D:\tmp\aqa\GapSkew\dicom\GapSkewRtPlans\RP.1.2.246.352.71.5.824327626427.478933.20170215160924.dcm""")
      //val file = new File("""D:\tmp\aqa\GapSkew\dicom\Study_1\RTIMAGE_01\RTIMAGE_003_2020-03-23T19-12-25.000.dcm""")
      val al = new AttributeList
      al.read(file)
      println("\n--------\n" + attributeListToString(al) + "\n--------\n")
      System.exit(99)
    }

    val fileList = {
      val dir = new File("src/test/resources")
      dir.listFiles.toSeq.filter(DicomFileUtilities.isDicomOrAcrNemaFile).take(5)
    }

    fileList.foreach(file => {
      val a = new AttributeList
      a.read(file)
      println("---------------------")
      println("File: " + file.getAbsolutePath)

      findAll(a, attr => ValueRepresentation.isTimeVR(attr.getVR)).foreach(attr => println("Time: " + attr))
      println
      findAll(a, attr => ValueRepresentation.isUniqueIdentifierVR(attr.getVR)).foreach(attr => println("UID: " + attr))
      println
      findAll(a, Set(TagFromName.SeriesDate, TagByName.FrameOfReferenceUID, TagByName.SeriesInstanceUID)).foreach(attr => println("UID: " + attr))
      println
      findAllSingle(a, TagFromName.SeriesTime).foreach(attr => println("UID: " + attr))

      if (false) {
        val s = DicomUtil.attributeListToString(a)
        println("\n\n\n\nfile: " + file.getAbsolutePath + "\n" + s)
      }
    })

    println("done")
    //    a.read("""D:\pf\eclipse\workspaceOxygen\ScalaUtil\src\test\resources\vessel_a.dcm""")
    //    val b = new AttributeList
    //    b.read("""D:\pf\eclipse\workspaceOxygen\ScalaUtil\src\test\resources\vessel_b.dcm""")
    //
    //    println("compareDicom(a,b) should be -1: " + compareDicom(a, b))
    //    println("compareDicom(b,a) should be  1: " + compareDicom(b, a))
    //    println("compareDicom(a,a) should be  0: " + compareDicom(a, a))
    //
    //    val copyA = clone(a)
    //    println("compareDicom(a,copyA) should be  0: " + compareDicom(a, copyA))
    //
    //    val aText = a.toString.replace('\u0000', ' ')
    //    val copyAText = copyA.toString.replace('\u0000', ' ')
    //
    //    println("Should be true: " + aText.equals(copyAText))
    //
    //    System.exit(99)
    //
    //    val name = "Smith^John    ^Q"
    //    println("DICOM name: " + name)
    //    val dpn = parseDicomPersonName(name)
    //    println("dpn: " + dpn)
  }

}
