package test.java

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.util.UMROGUID
import com.pixelmed.dicom.TransferSyntax
import com.pixelmed.dicom.OtherWordAttributeOnDisk
import com.pixelmed.dicom.SequenceAttribute
import java.io.File
import java.io.FileOutputStream

// Special for Kristy: Mon Nov 16 14:11:21 EST 2015

object Convert4Dto2D {

    val DEFAULT_TRANSFER_SYNTAX = TransferSyntax.ImplicitVRLittleEndian;

    private def makeUidAttr(tag: AttributeTag): Attribute = {
        val attr = AttributeFactory.newAttribute(tag)
        attr.addValue(UMROGUID.getUID)
        attr
    }

    private def destFile(srcFile: File): File = {
        val parent = srcFile.getParentFile
        val grandParent = parent.getParentFile
        val newParentName = "New" + parent.getName
        val newParentFile = new File(grandParent, newParentName)
        newParentFile.mkdirs
        val newFile = new File(newParentFile, srcFile.getName)
        newFile
    }

    private def getTransferSyntax(attributeList: AttributeList): String = {
        val ts = {
            val attr = attributeList.get(TagFromName.TransferSyntaxUID)
            if (attr != null) attr.getSingleStringValueOrDefault(DEFAULT_TRANSFER_SYNTAX) else DEFAULT_TRANSFER_SYNTAX
        }
        ts
    }

    private def fixDicom(dcmFile: File, replacementList: List[Attribute]): Unit = {
        val attributeList = new AttributeList
        attributeList.read(dcmFile)
        replacementList.map(rplc => attributeList.put(rplc))

        val newFile = destFile(dcmFile)
        attributeList.write(new FileOutputStream(newFile), TransferSyntax.ImplicitVRLittleEndian, true, true)
        println("    Created file " + newFile.getName)
    }

    val FrameOfReferenceUID = makeUidAttr(TagFromName.FrameOfReferenceUID)
    val StudyInstanceUID = makeUidAttr(TagFromName.StudyInstanceUID)

    private def fixDir(dir: File): Unit = {
        val t = TagFromName.SeriesInstanceUID

        println("Starting directory " + dir.getAbsolutePath)

        val replacements = List(
            makeUidAttr(TagFromName.SeriesInstanceUID),
            StudyInstanceUID,
            FrameOfReferenceUID)

        val dicomList = dir.listFiles.map(dcmFile => fixDicom(dcmFile, replacements))
        println("Finished directory " + dir.getAbsolutePath)
    }

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis
        val inputDicom = new File("D:\\tmp\\dicom4d\\im_0013.dcm")
        val mainList = new AttributeList
        mainList.read(inputDicom)

        val dataStart: Long = {
            val ob = (mainList.get(TagFromName.PixelData)).asInstanceOf[OtherWordAttributeOnDisk]
            ob.getByteOffset
        }

        val seq: SequenceAttribute = mainList.get(TagFromName.PerFrameFunctionalGroupsSequence).asInstanceOf[SequenceAttribute]

        def readSliceData(i: Int): Array[Short] = {
            null // TODO
        }

        def makeOneSlice(i: Int): Unit = {
            // TODO
        }

        def createSlices(attributeList: AttributeList): Unit = {
            (0 until seq.getNumberOfItems).map(i => makeOneSlice(i))
        }

        println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
        System.exit(0)
    }

}