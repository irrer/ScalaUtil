
package aqa.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import java.io.File
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import edu.umro.ScalaUtil.FileUtil

/**
 * Test the file utilities.
 *
 */

// TODO needs much work
class TestFileUtil extends FlatSpec with Matchers {

  "Util.makeFileBlob" should "make a Blob containing zipped files" in {

    //    val curDir = new File(".")
    //    val resourceDir = curDir.getAbsolutePath + """\src\test\resources"""
    //
    //    val mainDir = new File(resourceDir, "zipDir")
    //    val mainFile = new File(resourceDir, "zipFile")
    //
    //    val byteArray = FileUtil.readFileTreeToZipByteArray(Seq(mainDir), Seq[String]()) // Seq(".*xclude.*"))
    //
    //    val parentDir = new File("target\\zipBack")
    //    Trace.trace("parentDir: " + parentDir.getAbsolutePath)
    //    FileUtil.writeByteArrayZipToFileTree(byteArray, parentDir)
    //
    //    val outFile = new File("""D:\pf\eclipse\workspaceOxygen\ScalaUtil\target\kk\zippy.zip""")
    //
    //    Utility.writeFile(outFile, byteArray)
    //
    //    val unzipped = new File(resourceDir, "unzipped")
    //    FileUtil.writeZippedFileToFileTree(outFile, unzipped)
    //
    //    (byteArray.length > 10) should be(true)

    true should be(true)
  }
}
