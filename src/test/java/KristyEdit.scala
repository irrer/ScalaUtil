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

// Special for Kristy: Mon Nov 16 14:11:21 EST 2015

object KristyEdit {

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
        val mainDir = new File("\\\\rofile\\morfeusdata\\TG-132")
        val subDirList = mainDir.listFiles.filter(f => f.getName.size == 3).map(t => fixDir(t))
        println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
        System.exit(0)
    }

}