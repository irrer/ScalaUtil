package test.java

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.SequenceAttribute
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.DicomDict.DicomDict
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil

import java.io.File

/**
 * Look at all of the DICOM files in a given file tree and make a list of all distinct
 * attributes.  This is a way to generate a list for the DicomDict utility.
 */
object MakeDict {

  /** Put a wide assortment of DICOM files here. */
  private val mainDiX = new File("""D:\tmp\dict\Xdistinct""")
  private val mainDir = new File("""src\test\resources\MakeDict""")

  /** Accumulate a distinct list of attributes here. */
  private val master = new AttributeList

  private val contributingFiles = scala.collection.mutable.HashSet[String]()

  var fileCount = 0

  private def readFile(file: File) = {
    fileCount = fileCount + 1
    if ((fileCount % 1000) == 0)
      println("file count: " + fileCount)
    val al = new AttributeList
    // println("reading file: " + file.getAbsolutePath)
    al.read(file)
    al
  }


  /**
   * Show the attribute as java source code in the form:
   * public static final AttributeTag RTPlanTime = dict.getTagFromName("RTPlanTime"); // 300A,0007
   *
   * @param attr Attribute to show.
   */
  private def show(attr: Attribute): Unit = {
    val name = DicomDict.dict.getNameFromTag(attr.getTag)
    if (name != null)
      println("     public static final AttributeTag " + name + " = dict.getTagFromName(\"" + name + "\");" +
        " // " + attr.getTag.getGroup.formatted("%04x") + "," + attr.getTag.getElement.formatted("%04x"))
  }


  /**
   * Process one attribute.  If it is already on the list or has an unknown tag, then
   * ignore it.  Otherwise add it to the list.
   *
   * @param attr Attribute to examine
   */
  private def processAttr(attr: Attribute, file: File): Unit = {

    val tag = attr.getTag

    if ((master.get(tag) == null) && (DicomDict.dict.getNameFromTag(tag) != null)) {
      // show(attr)
      master.put(attr)
      contributingFiles.add(file.getAbsolutePath)
    }

    val vr = DicomDict.dict.getValueRepresentationFromTag(tag)
    if ((vr != null) && ValueRepresentation.isSequenceVR(vr)) {
      val alList = DicomUtil.alOfSeq(attr.asInstanceOf[SequenceAttribute])
      alList.foreach(al => {
        al.values().forEach(a => processAttr(a, file))
      })
    }

  }


  /**
   * Process one DICOM file.
   *
   * @param file File that contains DICOM.
   */
  private def processFile(file: File): Unit = {

    try {
      val al = readFile(file)
      al.values().forEach(a => processAttr(a, file))
    }

    catch {
      case _: Throwable => println("Ignoring file as non-DICOM: " + file.getAbsolutePath)
    }

  }

  /**
   * Descend the given file tree, recursively looking for all DICOM files.
   *
   * @param dir Top level file.
   */
  private def processTree(dir: File): Unit = {
    if (dir.isDirectory)
      FileUtil.listFiles(dir).foreach(processTree)
    else
      processFile(dir)
  }

  private def showContributingFiles(): Unit = {
    println("Contributing files:\n")

    val prefixLen = mainDir.getAbsolutePath.length + 1
    val nameList = contributingFiles.toSeq.sorted.map(n => n.drop(prefixLen).replace('\\', '/'))
    nameList.zipWithIndex.foreach(ni => println("cp " + ni._1 + "  distinct" + (ni._2 + 1).formatted("%02d") + ".dcm"))
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    processTree(mainDir)

    (0 until 5).foreach(_ => println("------------------------------------------------"))

    master.values().forEach(show)

    println("Number of distinct attributes: " + master.values().toArray.toSeq.size)

    (0 until 5).foreach(_ => println("=================================================================="))

    showContributingFiles()

    println("Elapsed ms: " + (System.currentTimeMillis - start))
  }

}