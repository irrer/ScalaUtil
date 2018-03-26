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

/**
 * Read a series from the given folder and set it up with valid InstanceNumber's.
 */

object FixInstanceNumber {

    /** Set this to the input directory. */
    val mainDir = new File("""D:\tmp\hen\output""")

    /** New files put here. */
    val outDir = new File(mainDir.getParent, mainDir.getName + "IN")
    outDir.mkdirs

    private def read(f: File) = {
        val attributeList = new AttributeList
        attributeList.read(f)
        attributeList
    }

    private def cmpr(a: AttributeList, b: AttributeList): Boolean = {
        def getPos(al: AttributeList) = al.get(TagFromName.ImagePositionPatient).getDoubleValues()(2)
        getPos(a) < getPos(b)
    }

    private def fixNum(attributeList: AttributeList, i: Int): Unit = {

        val a = attributeList.get(TagFromName.InstanceNumber)
        a.removeValues
        a.addValue(i + 1)

        val newFile = new File(outDir, i.formatted("IN%04d.dcm"))
        newFile.delete

        attributeList.write(new FileOutputStream(newFile), TransferSyntax.ImplicitVRLittleEndian, true, true)
        println("    Created file " + newFile.getName)
    }

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis
        val subDirList = mainDir.listFiles.map(f => read(f)).sortWith(cmpr).zipWithIndex.map(fi => fixNum(fi._1, fi._2))
        println("Done.  Elapsed ms: " + (System.currentTimeMillis - start))
        System.exit(0)
    }

}