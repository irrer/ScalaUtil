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
import com.pixelmed.dicom.DicomDictionary
import com.pixelmed.dicom.Attribute
import scala.util.Random
import com.pixelmed.dicom.IntegerStringAttribute

object ConstructMhd {

  case class Mhd(file: File) {
    val lineList = Utility.readFile(file).split("\n").toList
    def toKv(line: String) = (line.split(" ").head, line.split("=")(1).trim.split(" ").toSeq)
    private val kvMap = lineList.map(line => toKv(line)).toMap
    def get(key: String) = kvMap(key)

    val DimSize = get("DimSize").map(s => s.toInt)
    val ElementSpacing = get("ElementSpacing").map(s => s.toDouble)
    val Offset = get("Offset").map(s => s.toDouble)

    override def toString = {
      kvMap.keys.toSeq.map(k => k + " : " + kvMap(k).mkString(", ")).mkString("\n    ", "\n    ", "")
    }
  }

  def makeSeries(mhd: Mhd, image: Array[Byte], outDir: File, options: AttributeList) = {
    val transferSyntax = TransferSyntax.ImplicitVRLittleEndian
    val StudyInstanceUID = UMROGUID.getUID
    val SeriesInstanceUID = UMROGUID.getUID
    val FrameOfReferenceUID = UMROGUID.getUID
    val now = new Date
    val date = UMROGUID.dicomDate(now)
    val time = UMROGUID.dicomTime(now)
    val dateTime = date + time + ".000"

    val seriesNumber = options.get(TagFromName.SeriesNumber).getIntegerValues()(0)

    def makeSlice(sliceIndex: Int) = {
      val SOPInstanceUID = UMROGUID.getUID
      val al = new AttributeList
      val file = new File(outDir, (sliceIndex + 1).formatted("CT_" + seriesNumber + "_%03d.dcm"))
      if (file.exists) throw new RuntimeException("Was going to write to file " + file.getAbsolutePath + " but it already exists.")

      def makeM(tag: AttributeTag, text: Seq[String]): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        text.map(t => a.addValue(t))
        al.put(a)
      }

      def make(tag: AttributeTag, text: String): Unit = makeM(tag, Seq(text))

      def make0(tag: AttributeTag): Unit = makeM(tag, Seq[String]())

      def makeIntM(tag: AttributeTag, int: Seq[Int]): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        int.map(i => a.addValue(i))
        al.put(a)
      }

      def makeInt(tag: AttributeTag, int: Int): Unit = makeIntM(tag, Seq(int))

      def makeDblM(tag: AttributeTag, dbl: Seq[Double]): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        dbl.map(i => a.addValue(i))
        al.put(a)
      }

      def makeDbl(tag: AttributeTag, dbl: Double): Unit = makeDblM(tag, Seq(dbl))

      def makePixels: Unit = {
        val size = mhd.DimSize(0) * mhd.DimSize(1) * 2
        val pix = image.drop(size * sliceIndex).take(size)

        val a = new OtherByteAttribute(TagFromName.PixelData)
        a.setValues(pix)

        al.put(a)
      }

      val SliceLocation = mhd.Offset(2) + (sliceIndex * mhd.ElementSpacing(2))

      make(TagFromName.MediaStorageSOPClassUID, SOPClass.CTImageStorage)
      make(TagFromName.MediaStorageSOPInstanceUID, SOPInstanceUID)
      make(TagFromName.TransferSyntaxUID, transferSyntax)

      make(TagFromName.SpecificCharacterSet, "ISO_IR 100")
      makeM(TagFromName.ImageType, Seq("ORIGINAL", "PRIMARY", "AXIAL"))
      make(TagFromName.InstanceCreationDate, date)
      make(TagFromName.InstanceCreationTime, time)
      make(TagFromName.SOPClassUID, SOPClass.CTImageStorage)
      make(TagFromName.SOPInstanceUID, SOPInstanceUID)
      make(TagFromName.StudyDate, date)
      make(TagFromName.SeriesDate, date)
      make(TagFromName.AcquisitionDate, date)
      make(TagFromName.ContentDate, date)
      make(TagFromName.AcquisitionDateTime, dateTime)
      make(TagFromName.StudyTime, time)
      make(TagFromName.SeriesTime, time)
      make(TagFromName.AcquisitionTime, time)
      make(TagFromName.ContentTime, time)
      makeInt(TagFromName.AccessionNumber, 1)
      make(TagFromName.Modality, "CT")
      make(TagFromName.Manufacturer, "JimIrrer")
      make(TagFromName.InstitutionName, "Mich Medicine Rad Onc")
      make(TagFromName.InstitutionAddress, "Ann Arbor, MI")
      make(TagFromName.StationName, "none")
      make(TagFromName.StudyDescription, "none")
      make(TagFromName.SeriesDescription, "none")
      make(TagFromName.InstitutionalDepartmentName, "Rad Onc")
      make(TagFromName.OperatorsName, "Rocky Owen")
      make(TagFromName.ManufacturerModelName, "Fabricate from MHD")
      //make(TagFromName.PatientName, patientId)
      //make(TagFromName.PatientID, patientId)
      make(TagFromName.PatientBirthDate, "18000101")
      make(TagFromName.ReferringPhysicianName, "none")
      make(TagFromName.PatientSex, "O")
      make(TagFromName.SliceThickness, mhd.ElementSpacing(2).toString)
      makeDbl(TagFromName.KVP, 120)
      makeDbl(TagFromName.SpacingBetweenSlices, mhd.ElementSpacing(2).toDouble)
      make(TagFromName.DeviceSerialNumber, "001")
      make(TagFromName.SoftwareVersions, "irrer 0.0.2")
      make(TagFromName.PatientPosition, "HFS")
      make(TagFromName.AcquisitionType, "SPIRAL")
      make(TagFromName.StudyInstanceUID, StudyInstanceUID)
      make(TagFromName.SeriesInstanceUID, SeriesInstanceUID)
      make(TagFromName.StudyID, "none")
      makeInt(TagFromName.InstanceNumber, sliceIndex + 1)
      makeDblM(TagFromName.ImagePositionPatient, Seq(mhd.Offset(0), mhd.Offset(1), SliceLocation))
      makeM(TagFromName.ImageOrientationPatient, Seq("1", "0", "0", "0", "1", "0"))
      make(TagFromName.FrameOfReferenceUID, FrameOfReferenceUID)
      if (true) {
        val a = AttributeFactory.newAttribute(TagFromName.PositionReferenceIndicator)
        a.addValue(0.toLong)
        al.put(a)
      } else makeIntM(TagFromName.PositionReferenceIndicator, Seq[Int]())
      makeDbl(TagFromName.SliceLocation, SliceLocation)
      makeInt(TagFromName.SamplesPerPixel, 1)
      make(TagFromName.PhotometricInterpretation, "MONOCHROME2")
      makeInt(TagFromName.Rows, mhd.DimSize(1))
      makeInt(TagFromName.Columns, mhd.DimSize(0))
      makeM(TagFromName.PixelSpacing, mhd.ElementSpacing.take(2).map(d => d.toString))
      makeInt(TagFromName.BitsAllocated, 16)
      makeInt(TagFromName.BitsStored, 16)
      makeInt(TagFromName.HighBit, 15)
      makeInt(TagFromName.PixelRepresentation, 1)
      makeInt(TagFromName.AcquisitionNumber, 1)
      makeDbl(TagFromName.RescaleIntercept, 0)
      makeDbl(TagFromName.RescaleSlope, 1)

      makePixels

      // put in user options
      options.values.toArray.toSeq.map(a => al.put(a.asInstanceOf[Attribute]))

      FileMetaInformation.addFileMetaInformation(al, transferSyntax, "IrrerMHD");

      DicomUtil.writeAttributeList(al, file)
      println("Created " + file.getAbsolutePath)
    }

    for (sliceIndex <- (0 until mhd.DimSize(2))) {
      makeSlice(sliceIndex)
    }
  }

  private val readMe = {
    """
FOR RESEARCH ONLY.  NOT FOR CLINICAL USE.

----------------------------------------
This utility is run from the DOS command line and creates a DICOM
series given an MHD file, image file, and list of options.

This software requires Java to be installed with a minimum of version 8.

Command usage:

    constructmhd [mhdfile] [imagefile] [outputdir] "option1:value1" "option2:value2" ...

Enclosing option:value pairs in quotes allows special characters in the value.

The following options are supported.  These all represent DICOM tags and
may be useful for constructing the DICOM the way it is needed.  If they are
not specified then a default value will be used.

    StudyDescription
    SeriesDescription
    StudyID
    PatientName : standard syntax is LastName^FirstName
    PatientID
    PatientPosition : Defaults to HFS.  (must be one of HFS HFP HFDR HFDL FFDR FFDL FFP FFS)
    SeriesNumber : must be an integer, usually a small number unique to the series.  Used to name DICOM files.

Example:

    constructmhd foo.mhd images.raw C:\Users\rockyo\dicom "PatientID:$lung001" "SeriesDescription:left lung" "SeriesNumber:5"

Files would be created with CT_[SeriesNumber]_[InstanceNumber].dcm , as in:

    CT_5_001.dcm
    CT_5_002.dcm
    CT_5_003.dcm
    CT_5_004.dcm
    .
    .
    .

This software was written by Jim Irrer (irrer@med.umich.edu).

A free DICOM anonymizer/viewer (also by Jim Irrer) is
available at: https://github.com/irrer/DICOMClient#user-content-dicom-for

FOR RESEARCH ONLY.  NOT FOR CLINICAL USE.
"""
  }

  private def usage(msg: String) = {
    println(msg)
    println(readMe)
    System.exit(1)
  }

  private val dict = new DicomDictionary
  val rand = new Random

  val defaultPatId = "$" + ("00000000" + rand.nextLong.toString).takeRight(8)

  val defaultSeriesNumber = (rand.nextInt(1000) + 1) % 1000

  /**
   * Parse the user options into an AttributeList. Require it to contain a PatientID and PatientName.
   */
  private def getOptions(args: Array[String]): AttributeList = {

    val al = new AttributeList

    // ensure that there is a SeriesNumber so we can name files.
    val SeriesNumber = AttributeFactory.newAttribute(TagFromName.SeriesNumber)
    SeriesNumber.addValue(defaultSeriesNumber)
    al.put(SeriesNumber)

    /**
     * Make an attribute from a name:value pair
     */
    def argToAttr(text: String) = {
      val sep = text.indexOf(':')

      val name = text.substring(0, sep)
      val value = text.substring(sep + 1)

      val attr = AttributeFactory.newAttribute(dict.getTagFromName(name))
      if (attr.isInstanceOf[IntegerStringAttribute])
        attr.addValue(value.toInt)
      else
        attr.addValue(value)
      al.put(attr)
      attr
    }

    args.toSeq.drop(3).map(a => argToAttr(a))

    def addPatId(patId: String) = {
      val attr = AttributeFactory.newAttribute(TagFromName.PatientID)
      attr.addValue(patId)
      al.put(attr)
    }

    def addPatName(patName: String) = {
      val attr = AttributeFactory.newAttribute(TagFromName.PatientName)
      attr.addValue(patName)
      al.put(attr)
    }

    (al.get(TagFromName.PatientID), al.get(TagFromName.PatientName)) match {
      case (null, null) => {
        addPatId(defaultPatId)
        addPatName(defaultPatId)
      }
      case (null, patName) => addPatId(patName.getSingleStringValueOrDefault(defaultPatId))
      case (patId, null) => addPatName(patId.getSingleStringValueOrDefault(defaultPatId))
      case _ =>
    }
    al
  }

  def restrict = println("This software is for research only.  Not for clinical use.")

  def main(args: Array[String]): Unit = {
    try {
      restrict
      val start = System.currentTimeMillis
      if (args.size == 0) usage("Help")
      if (args.size < 3) usage("Must give at least two files, an MHD file and an image file, followed by the output folder.")
      val mhdFileName = args(0)
      val imageFileName = args(1)

      val options = getOptions(args)

      val mhdFile = new File(mhdFileName)
      val imageFile = new File(imageFileName)

      if (!mhdFile.canRead) usage("Can not read MHD file: " + mhdFile.getAbsolutePath)
      if (!imageFile.canRead) usage("Can not read image file: " + imageFile.getAbsolutePath)

      // output directory named after MHD file
      val outDir = new File(args(2))
      outDir.mkdirs
      println("Using MHD file: " + mhdFile.getAbsolutePath + "    image file: " + imageFile.getAbsolutePath + "    output: " + outDir.getAbsolutePath)
      val mhd = new Mhd(mhdFile)
      println("mhd: " + mhd)
      val expectedImageSize = mhd.DimSize.map(i => i.toLong).product * 2
      val imageBytes = Utility.readBinFile(imageFile)
      if (imageBytes.size != expectedImageSize) {
        usage("The MHD file says that the image should contain " + expectedImageSize + " bytes, but actually has " + imageBytes.size + "  Do you have the right MHD paired with the right image file?")
      }
      makeSeries(mhd, imageBytes, outDir, options)

      println("Elapsed ms: " + (System.currentTimeMillis - start))
    } catch {
      case t: Throwable =>
        usage("Unexpected error: " + t.getMessage)
        t.printStackTrace
    }
    restrict
  }

}
