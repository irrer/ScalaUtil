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

object DicomUtil {

  val dictionary = new DicomDictionary

  private val minorIndent = "  "
  private val indentText = minorIndent + minorIndent

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
      val element = tag.getElement().formatted("%04x")
      val group = tag.getGroup().formatted("%04x")
      val vrText: String = if (vr == null) "??" else new String(vr)
      group + "," + element + " " + vrText
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
   * Recursively search the given attribute list for all instances of the given tag.
   */
  def findAll(attributeList: AttributeList, tag: AttributeTag): List[Attribute] = {
    def tagOfList(al: AttributeList): Array[Attribute] = if (al.get(tag) == null) Array[Attribute]() else Array(al.get(tag))

    def childLists(al: AttributeList): Array[AttributeList] = {
      val seqAList = al.values.toArray.filter(a => a.isInstanceOf[SequenceAttribute]).map(sa => sa.asInstanceOf[SequenceAttribute])
      seqAList.map(sa => (0 until sa.getNumberOfItems).map(i => sa.getItem(i).getAttributeList)).flatten
    }

    def find(al: AttributeList): Array[Attribute] = childLists(al).map(c => find(c)).foldLeft(tagOfList(al))(_ ++ _)

    find(attributeList).toList
  }

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
   * Self test.
   */
  def main(args: Array[String]): Unit = {

    val a = new AttributeList
    a.read("""D:\tmp\mrct2\input\Vessel\1.3.6.1.4.1.22361.48658618118952.1460113624.1461353992992.986.dcm""")
    val b = new AttributeList
    b.read("""D:\tmp\mrct2\input\Vessel\1.3.6.1.4.1.22361.48658618118952.1460113624.1461353992992.988.dcm""")

    println("compareDicom(a,b): " + compareDicom(a, b))
    println("compareDicom(b,a): " + compareDicom(b, a))
    println("compareDicom(a,a): " + compareDicom(a, a))
    System.exit(99)

    val name = "Smith^John    ^Q"
    println("DICOM name: " + name)
    val dpn = parseDicomPersonName(name)
    println("dpn: " + dpn)
  }

}