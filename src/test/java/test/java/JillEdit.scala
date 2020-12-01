package test.java

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import java.io.File
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.TransferSyntax
import java.io.FileOutputStream
import edu.umro.ScalaUtil.FileUtil
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.SOPClass
import edu.umro.ScalaUtil.DicomUtil

// Special for Kristy: Mon Nov 16 14:11:21 EST 2015

object JillEdit {

  val DEFAULT_TRANSFER_SYNTAX = TransferSyntax.ImplicitVRLittleEndian;

  private def makeAttr(tag: AttributeTag, value: String): Attribute = {
    val attr = AttributeFactory.newAttribute(tag)
    attr.addValue(value)
    attr
  }

  private def makeUidAttr(tag: AttributeTag): Attribute = makeAttr(tag, UMROGUID.getUID)

  private def makeReferencedRTPlanSequence = {
    val al = new AttributeList
    al.put(makeAttr(TagFromName.ReferencedSOPClassUID, SOPClass.RTPlanStorage))
    al.put(makeAttr(TagFromName.ReferencedSOPInstanceUID, "1.2.246.352.71.5.116071937349.789597.20201124150130"))
    val seqAt = AttributeFactory.newAttribute(TagFromName.ReferencedRTPlanSequence).asInstanceOf[SequenceAttribute]
    seqAt.addItem(al)
    seqAt
  }

  private def fixDicom(outDir: File, dcmFile: File, replacementList: Seq[Attribute]): Unit = {
    val attributeList = new AttributeList
    attributeList.read(dcmFile)
    replacementList.map(at => attributeList.put(at)) // actually replace attributes
    val newFile = new File(outDir, dcmFile.getName)
    DicomUtil.writeAttributeListToFile(attributeList, newFile, "JillEdit")
    println("    Created file " + newFile.getName)
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    val inDir = new File("""D:\tmp\aqa\tmp\Jill\DeveloperMode\DM_images""")
    println("Using input dir: " + inDir.getAbsolutePath)

    val outDir = new File("""D:\tmp\aqa\tmp\Jill\DeveloperMode\DM_imagesOut""")
    println("Output dir: " + outDir.getAbsolutePath)
    FileUtil.deleteFileTree(outDir)
    outDir.mkdirs
    Thread.sleep(250)

    val replacementList = Seq(
      makeUidAttr(TagFromName.StudyInstanceUID),
      makeUidAttr(TagFromName.SeriesInstanceUID),
      makeUidAttr(TagFromName.FrameOfReferenceUID),
      makeAttr(TagFromName.DeviceSerialNumber, "3050"),
      makeReferencedRTPlanSequence)

    val fileList = FileUtil.listFiles(inDir)

    fileList.map(inFile => fixDicom(outDir, inFile, replacementList))
    println("Output dir: " + outDir.getAbsolutePath)

    println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
    System.exit(0)
  }

}