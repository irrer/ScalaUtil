package edu.umro.ScalaUtil

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.AttributeTagAttribute
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.DicomDictionary
import java.util.HashSet
import com.pixelmed.dicom.ValueRepresentation
import com.pixelmed.dicom.SOPClassDescriptions
import com.pixelmed.dicom.OtherWordAttribute
import com.pixelmed.dicom.OtherFloatAttribute
import com.pixelmed.dicom.OtherByteAttribute
import scala.collection.mutable.ArrayBuffer
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.SOPClass
import java.io.ByteArrayOutputStream
import com.pixelmed.dicom.DicomOutputStream
import com.pixelmed.dicom.TransferSyntax
import java.io.ByteArrayInputStream
import com.pixelmed.dicom.DicomInputStream
import java.io.OutputStream
import java.io.File
import java.io.FileOutputStream
import java.text.SimpleDateFormat
import java.util.zip.ZipOutputStream
import java.util.zip.ZipEntry
import resource.managed
import java.util.zip.ZipInputStream
import scala.annotation.tailrec
import com.pixelmed.dicom.FileMetaInformation
import java.util.Date

object DicomUtil {

  val dictionary = new DicomDictionary

  private val minorIndent = "  "
  private val indentText = minorIndent + minorIndent

  /** DICOM compatible date format. */
  val dicomDateFormat = new SimpleDateFormat("yyyyMMdd")

  /** DICOM compatible time format. */
  val dicomTimeFormat = new SimpleDateFormat("HHmmss.SSS")

  val dicomTimeFormatSimple = new SimpleDateFormat("HHmmss")

  /**
   * Parse a text string in DICOM time format and return ms.  On failure to parse return None.
   */
  def parseDicomTime(text: String): Option[Long] = {
    val parts = text.split('.')

    try {
      val upper = dicomTimeFormatSimple.parse(parts(0))
      val ms: Long = if (parts.size > 1) {
        val uS = ((parts(1) + "000000")).take(6).toDouble
        (uS / 1000).round.toLong
      } else 0

      Some(upper.getTime + ms)
    } catch {
      case t: Throwable => None
    }
  }

  /** Used for converting a DICOM date+time pair into a <code>Date</code> */
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
        val t2 = if (t.contains('.')) t + "000"
        else t + ".000"
        t2.take(10)
      }
      Some(dicomDateTimeFormat.parse(dateText + timeText))
    } catch {
      case e: Throwable => None
    }
  }

  /**
   * List of value representations that can be displayed as strings in the
   * text version of the preview.
   */
  private val TEXTUAL_VR = List(
    ValueRepresentation.AE, ValueRepresentation.AS, ValueRepresentation.CS, ValueRepresentation.DA,
    ValueRepresentation.DS, ValueRepresentation.DT, ValueRepresentation.FL, ValueRepresentation.FD,
    ValueRepresentation.IS, ValueRepresentation.LO, ValueRepresentation.LT, ValueRepresentation.PN,
    ValueRepresentation.SH, ValueRepresentation.SL, ValueRepresentation.SS, ValueRepresentation.ST,
    ValueRepresentation.TM, ValueRepresentation.UI, ValueRepresentation.UL, ValueRepresentation.US,
    ValueRepresentation.UT, ValueRepresentation.XS, ValueRepresentation.XO);

  /** A quickly searchable list of value representations. */
  private val vrSet = new HashSet[String]

  TEXTUAL_VR.map(vr => vrSet.add(new String(vr)))

  /**
   * Show a byte value as humanly readable as possible. If it is a displayable
   * ASCII character, then show that, otherwise show the hex value (as in
   * 0xfe).
   *
   * @param i
   *
   * @return
   */
  private def byteToHuman(i: Int): String = {
    val b = i & 255;
    if ((b >= 32) && (b <= 126)) b.asInstanceOf[Char].toString else b.formatted("0x%x")
  }

  /**
   * Format an attribute tag as a string.
   */
  def formatAttrTag(tag: AttributeTag) = tag.getGroup.formatted("%04x") + "," + tag.getElement.formatted("%04x")

  /**
   * Convert a single non-sequence attribute to a human readable text format.
   *
   * @param attribute
   *            Attribute to format.
   *
   * @return String version of attribute.
   */
  def attributeToString(attribute: Attribute, indentLevel: String): String = {
    val tag = attribute.getTag();
    val line = new StringBuffer();
    val vrDict = dictionary.getValueRepresentationFromTag(tag)
    val vr = if (vrDict == null) attribute.getVR() else vrDict
    val VALUE_SEPARATOR = " \\ "
    val MAX_LINE_LENGTH = 500

    def foldStringList(list: List[String], valueSeparator: String): String = list.foldLeft("")((t, v) =>
      t.size match {
        case 0 => v
        case size if (size > MAX_LINE_LENGTH) => t
        case _ => t + valueSeparator + v
      })

    def tagDetails: String = {
      val vrText: String = if (vr == null) "??" else new String(vr)
      formatAttrTag(tag) + " " + vrText
    }

    def toTextualVR: String = {
      val classSop: String = {
        val value = attribute.getSingleStringValueOrNull
        if ((value != null) && (ValueRepresentation.isUniqueIdentifierVR(vr)) && (SOPClassDescriptions.getDescriptionFromUID(value).length() > 0)) { " (" + SOPClassDescriptions.getDescriptionFromUID(value) + ")" } else ""
      }
      val text = foldStringList(
        (if (attribute.getStringValues == null) List("<null>")
        else attribute.getStringValues.toList), VALUE_SEPARATOR)
      (text + classSop)
    }

    def toAttributeTagVR(attr: AttributeTagAttribute): String = {
      val textList = attr.getAttributeTagValues.map(t => {
        if (dictionary.getNameFromTag(t) == null) ":<unknown>"
        else ":" + dictionary.getNameFromTag(t)
      })

      foldStringList(textList.toList, VALUE_SEPARATOR)
    }

    def limitedCopy[A](raw: Array[A], max: Int): List[A] = {
      if (raw.size > max) {
        val cooked = ArrayBuffer[A]()
        (0 until max).map(i => cooked :+ raw(i))
        cooked.toList
      } else
        raw.toList
    }

    def toOtherByte(attr: OtherByteAttribute): String = {
      val data = limitedCopy(attr.getByteValues, MAX_LINE_LENGTH / 3)
      foldStringList(data.map(d => (d.toInt & 0xff).formatted("0x%x")), " ")
    }

    def toOtherFloat(attr: OtherFloatAttribute): String = {
      val data = limitedCopy(attr.getFloatValues, MAX_LINE_LENGTH / 3)
      foldStringList(data.map(d => d.toString), " ")
    }

    def toOtherWord(attr: OtherWordAttribute): String = {
      val data = limitedCopy(attr.getShortValues, MAX_LINE_LENGTH / 3)
      foldStringList(data.map(d => ((d & 0xffff) / 256).formatted("0x%x")), " ")
    }

    def toSequenceAttribute(attr: SequenceAttribute): String = {
      val size = attr.getNumberOfItems
      val textList = (0 until size).map(i => {
        (if (i == 0) "\n" else "") +
          indentLevel + minorIndent + "Item " + (i + 1) + " / " + size +
          "\n" + attributeListToString(attr.getItem(i).getAttributeList, indentLevel + indentText)
      })
      val text = textList.foldLeft("")((t, a) => t + a)
      if (text.endsWith("\n")) text.subSequence(0, text.size - 1).toString else text
    }

    val valueText: String =
      if (attribute.isInstanceOf[SequenceAttribute]) {
        toSequenceAttribute(attribute.asInstanceOf[SequenceAttribute])
      } else {
        attribute match {
          case _ if (vr != null) && vrSet.contains(new String(vr)) => toTextualVR
          case attr: AttributeTagAttribute => toAttributeTagVR(attr)
          case attr: OtherByteAttribute => toOtherByte(attr)
          case attr: OtherFloatAttribute => toOtherFloat(attr)
          case attr: OtherWordAttribute => toOtherWord(attr)
          case attr: SequenceAttribute => toSequenceAttribute(attr)
          case _ => "unknown"
        }
      }.replace('\n', ' ').replace('\0', ' ').replace('\r', ' ') // remove funky characters

    val tagName = if (dictionary.getNameFromTag(tag) == null) "<unknown>" else dictionary.getNameFromTag(tag)

    indentLevel + tagDetails + "  " + tagName + ": " + valueText
  }

  private def attributeListToString(attributeList: AttributeList, indent: String): String = {
    attributeList.keySet.toArray.toList.map(tag => {
      val t = tag
      val obj = tag.asInstanceOf[Object]
      val a = attributeList.get(t).asInstanceOf[Attribute]
      attributeToString(a, indent)
    }).foldLeft("")((t, a) => t + a + "\n")
  }

  def attributeListToString(attributeList: AttributeList): String = attributeListToString(attributeList, "")

  /**
   * Represent the components of a person name (Value Representation PN) in DICOM format.
   *
   *  Smith^John^Q --> John Q Smith
   */
  case class DicomPersonName(familyNameComplex: Option[String], givenNameComplex: Option[String], middleName: Option[String], namePrefix: Option[String], nameSuffix: Option[String]) {
    def partToStr(part: Option[String]): String = if (part.isDefined) { part.get + " " } else ""
    override def toString: String = (partToStr(namePrefix) + partToStr(givenNameComplex) + partToStr(middleName) + partToStr(familyNameComplex) + partToStr(nameSuffix)).replaceAll("  *", " ").trim
  }

  /**
   * Given a person name (Value Representation PN) in DICOM format, break it down into its components.
   */
  def parseDicomPersonName(text: String): DicomPersonName = {
    val pn = text.split("\\^")
    def getPn(i: Int): Option[String] = if (pn.size > i) Some(pn(i)) else None

    new DicomPersonName(getPn(0), getPn(1), getPn(2), getPn(3), getPn(4))
  }

  /**
   * Compare two DICOM files for the purpose of sorting them in the order that humans expect.
   */
  def compareDicom(a: AttributeList, b: AttributeList): Int = {

    def nullAttrCheck(tag: AttributeTag): Either[Int, (Attribute, Attribute)] = {

      (a.get(tag), b.get(tag)) match {
        case (null, null) => Left(0)
        case (null, _) => Left(-1)
        case (_, null) => Left(1)
        case (aAttr, bAttr) => Right(aAttr, bAttr)
      }
    }

    def compareString(tag: AttributeTag)(dummy: Any): Int = {

      def comprVal(aAttr: Attribute, bAttr: Attribute): Int = {
        (aAttr.getSingleStringValueOrNull, bAttr.getSingleStringValueOrNull) match {
          case (null, null) => 0
          case (null, _) => -1
          case (_, null) => 1
          case (aVal, bVal) => aVal.compareTo(bVal)
        }
      }

      nullAttrCheck(tag) match {
        case Left(c) => c
        case Right((aAttr, bAttr)) => comprVal(aAttr, bAttr)
      }

    }

    def compareDouble(tag: AttributeTag, index: Int)(dummy: Any): Int = {

      def comprVal(aAttr: Attribute, bAttr: Attribute): Int = {
        (aAttr.getDoubleValues, bAttr.getDoubleValues) match {
          case (null, null) => 0
          case (null, _) => -1
          case (_, null) => 1
          case (aVal, _) if (aVal.size <= index) => -1
          case (_, bVal) if (bVal.size <= index) => 1
          case (aVal, bVal) => aVal(index).compareTo(bVal(index))
        }
      }

      nullAttrCheck(tag) match {
        case Left(c) => c
        case Right((aAttr, bAttr)) => comprVal(aAttr, bAttr)
      }

    }

    lazy val seq: Seq[(Any) => Int] = Seq(
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
      compareString(TagFromName.SOPInstanceUID))

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
   * @param source
   *            List to copy.
   *
   * @return Copy of list.
   *
   */
  def clone(source: AttributeList): AttributeList = {
    val dest = new AttributeList

    val transferSyntaxAttr = source.get(TagFromName.TransferSyntaxUID)
    val transferSyntax = {
      val ts = source.get(TagFromName.TransferSyntaxUID)
      if ((ts != null) && ts.getStringValues.nonEmpty)
        ts.getStringValues.head
      else
        TransferSyntax.ExplicitVRLittleEndian; // DEFAULT_TRANSFER_SYNTAX;
    }
    val byteArrayOutputStream = new ByteArrayOutputStream
    val dicomOutputStream = new DicomOutputStream(byteArrayOutputStream, transferSyntax, transferSyntax);
    source.write(dicomOutputStream);

    val byteArrayInputStream = new ByteArrayInputStream(byteArrayOutputStream.toByteArray)
    dest.read(new DicomInputStream(byteArrayInputStream))

    dest
  }

  /**
   * Get the attribute lists of a sequence attribute.
   */
  def seqToAttr(al: AttributeList, tag: AttributeTag): Seq[AttributeList] = {
    val seq = (al.get(tag)).asInstanceOf[SequenceAttribute]
    (0 until seq.getNumberOfItems).map(i => seq.getItem(i).getAttributeList)
  }

  /**
   * Get the attribute lists of a sequence attribute.
   */
  def alOfSeq(al: SequenceAttribute): Seq[AttributeList] = {
    (0 until al.getNumberOfItems).map(i => al.getItem(i).getAttributeList)
  }

  /**
   * Get all instances of attributes with a tag on the given list by searching the given <code>AttributeList</code> recursively.
   */
  def findAll(attributeList: AttributeList, tagSet: Set[AttributeTag]): IndexedSeq[Attribute] = {

    def childSeq(al: AttributeList): IndexedSeq[AttributeList] = {
      val seqList = al.values.toArray.filter(at => at.isInstanceOf[SequenceAttribute]).map(at => at.asInstanceOf[SequenceAttribute])
      val alListList = seqList.map(seq => (0 until seq.getNumberOfItems).map(i => seq.getItem(i).getAttributeList)).flatten
      alListList.toIndexedSeq
    }

    val atList = attributeList.values.toArray.toList.toIndexedSeq.map(at => at.asInstanceOf[Attribute])
    val listOfInterest = atList.filter(at => tagSet.contains(at.getTag))
    val all = listOfInterest ++ childSeq(attributeList).map(child => findAll(child, tagSet)).flatten
    all
  }

  def findAllSingle(attributeList: AttributeList, tag: AttributeTag): IndexedSeq[Attribute] = findAll(attributeList, Set(tag))

  /**
   * Write and attribute list to an output stream, preserving the TransferSyntaxUID if specified.  Flush and
   * close the output stream.  Throw an exception if there is an IO error.
   */
  def writeAttributeList(attributeList: AttributeList, outputStream: OutputStream, sourceApplication: String): Unit = {
    val transferSyntax: String = {
      val ts = attributeList.get(TagFromName.TransferSyntaxUID)
      if ((ts != null) && (ts.getSingleStringValueOrNull != null)) ts.getSingleStringValueOrNull
      else TransferSyntax.ImplicitVRLittleEndian
    }

    FileMetaInformation.addFileMetaInformation(attributeList, transferSyntax, sourceApplication)
    attributeList.write(outputStream, transferSyntax, true, true)
    outputStream.flush
    outputStream.close
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
      val transferSyntax: String = {
        val ts = attributeList.get(TagFromName.TransferSyntaxUID)
        if ((ts != null) && (ts.getSingleStringValueOrNull != null)) ts.getSingleStringValueOrNull
        else TransferSyntax.ImplicitVRLittleEndian
      }

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
    managed(new ZipOutputStream(byteArrayOutputStream)) acquireAndGet {
      zipOut =>
        alList.map(al => addOneAlToZip(al, zipOut))
    }
    byteArrayOutputStream.toByteArray
  }

  /**
   * Read a list of attribute lists from a zipped byte array.
   */
  def zippedByteArrayToDicom(data: Array[Byte]): Seq[AttributeList] = {

    val byteArrayInputStream = new ByteArrayInputStream(data)

    @tailrec
    def next(zipIn: ZipInputStream, alList: Seq[AttributeList]): Seq[AttributeList] = {
      val entry = zipIn.getNextEntry
      if (entry == null)
        alList
      else {
        if (entry.isDirectory)
          next(zipIn, alList)
        else {
          val al = new AttributeList
          val attrList: Seq[AttributeList] = try {
            val dicomInputStream = new DicomInputStream(zipIn)
            al.read(dicomInputStream)
            Seq(al)
          } catch {
            // if there is an error, then assume that this is not a DICOM file and ignore it
            case t: Throwable => Seq[AttributeList]()
          }
          next(zipIn, alList ++ attrList)
        }
      }
    }

    val list = managed(new ZipInputStream(byteArrayInputStream)) acquireAndGet {
      zipIn => next(zipIn, Seq[AttributeList]())
    }
    list
  }

  /**
   * Remove members of the sequence that match according to the given function.
   *
   * @param al: Contains the main SequenceAttribute
   *
   * @param seqAttrTag: Tag of main SequenceAttribute
   *
   * @param identifyForRemoval: Returns true for each attribute list that should be removed.
   */
  def removeSeq(al: AttributeList, seqAttrTag: AttributeTag, identifyForRemoval: (AttributeList) => Boolean): Seq[AttributeList] = {
    val listPair = DicomUtil.seqToAttr(al, seqAttrTag).partition(identifyForRemoval)
    val remove = listPair._1
    val keep = listPair._2
    al.remove(seqAttrTag)
    val newSeq = new SequenceAttribute(seqAttrTag)
    keep.map(k => newSeq.addItem(k))
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
        findAllSingle(rtplan, TagFromName.ManufacturerModelName).
          map(a => a.getSingleStringValueOrEmptyString.toUpperCase.trim).
          distinct.
          filterNot(tmt => tmt.equals("")).
          filterNot(tmt => tmt.equals(mainMMN)).
          toSeq

      val tmt = ManufacturerModelNameList match {
        case _ if ManufacturerModelNameList.isEmpty => None
        case _ if truebeamNameList.contains(ManufacturerModelNameList.head) => Some(Truebeam)
        case _ if halcyonNameList.contains(ManufacturerModelNameList.head) => Some(Halcyon)
        case _ if clinacCNameList.contains(ManufacturerModelNameList.head) => Some(ClinacC)
        case _ => None
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
   * Self test.
   */
  def main(args: Array[String]): Unit = {

    val a = new AttributeList
    a.read("""D:\pf\eclipse\workspaceOxygen\ScalaUtil\src\test\resources\vessel_a.dcm""")
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
    //    val aText = a.toString.replace('\0', ' ')
    //    val copyAText = copyA.toString.replace('\0', ' ')
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