package edu.umro.ScalaUtil.DicomSplit

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.util.UMROGUID

import java.io.File

/**
  * Split a 4D DICOM image series into several 3D image series.
  *
  * See <code>usage</code> for more details.
  */

object DicomSplit extends Logging {
  private val zero: Double = {
    if (System.currentTimeMillis() == 0)
      1.0
    else
      0.0
  }
  private def makeTag(text: String): AttributeTag = {
    val ge = text.split(",").map(n => Integer.parseInt(n.toUpperCase(), 16))
    val tag = new AttributeTag(ge.head, ge(1))

    val name = {
      val n = TagByName.dict.getNameFromTag(tag)
      if (n == null) "NA" else n
    }
    println(s"Using differentiating tag $tag : $name")
    tag
  }

  private def makeDicomFile(file: File, diffTag: AttributeTag): Option[DicomFile] = {
    try {
      val al = new AttributeList
      al.read(file)
      val SliceLocation = al.get(TagByName.SliceLocation).getDoubleValues.head + zero
      val diffAttr = DicomUtil.findAllSingle(al, diffTag).head.getSingleStringValueOrNull
      val diffText = new String(diffAttr)
      Some(DicomFile(file, SliceLocation, diffText))
    } catch {
      case _: Throwable =>
        println("Ignoring file " + file.getAbsolutePath)
        None
    }
  }

  /**
    * Make a new version of the DICOM file based on the original, changing the SeriesInstanceUID and SOPInstanceUID.
    * @param df Original file.
    * @param dir Destination directory.
    * @param seriesUid New SeriesInstanceUID to use.
    */
  private def makeNew(df: DicomFile, dir: File, seriesUid: String): Unit = {
    val al = new AttributeList
    al.read(df.file)

    val seriesUidAttr = al.get(TagByName.SeriesInstanceUID)
    seriesUidAttr.removeValues()
    seriesUidAttr.addValue(seriesUid)

    val sopUidAttr = al.get(TagByName.SOPInstanceUID)
    sopUidAttr.removeValues()
    sopUidAttr.addValue(UMROGUID.getUID)

    val file = new File(dir, df.file.getName)
    DicomUtil.writeAttributeListToFile(al, file, "DicomSplit")
    println(df.diff + " : " + df.SliceLocation.formatted("%12.6f") + " new file " + file.getAbsolutePath)
  }

  private def writeGroup(dirName: String, list: Seq[DicomFile]): Unit = {
    val dir = new File(list.head.file.getParentFile, dirName)
    FileUtil.deleteFileTree(dir)
    dir.mkdirs()
    val seriesUid = UMROGUID.getUID
    list.sortBy(_.SliceLocation).foreach(df => makeNew(df, dir, seriesUid))
  }

  private def usage(status: Int): Unit = {
    println("""
        |Split a 4D DICOM image series into multiple 3D series.
        |
        |Usage: dir tag [split]
        |
        |   dir: source directory containing all the files for a single series.
        |
        |   tag: attribute tag to use to differentiate between new series.
        |
        |   [split]: Optionally specify the word "split" (case insensitive).
        |       When specified, the new files get created.  If not, then it
        |       will only look at files and print information on how they
        |       would be split up.
        |
        |   Examples:
        |       Read the files in mydir and list the groups that they would
        |       be split into, along with their slice locations.  Use 0018,0022
        |       CS ScanOptions (0018,0022) to determine which series they
        |       belong to.
        |
        |           dicomsplit mydir 0018,0022
        |
        |       Read the files in somedir and, after showing the groups, split
        |       them into separate series using
        |       NominalPercentageOfCardiacPhase  (0020,9241 )
        |
        |           dicomsplit somedir 0020,9241 split
        |
        |""".stripMargin)
    System.exit(status)
  }

  def main(args: Array[String]): Unit = {

    if ((args.length < 2) || (args.length > 3))
      usage(0)

    try {
      val dir = new File(args(0))
      println("Using directory: " + dir.getAbsolutePath)
      val tag = makeTag(args(1))
      val list = FileUtil.listFiles(dir).flatMap(file => makeDicomFile(file, tag))
      val groups = list.groupBy(_.diff)

      def show(grp: String): String = {
        grp.formatted("%6s") + " : " + groups(grp).size.formatted("%5d") + " : " + groups(grp).map(_.SliceLocation).sorted.mkString(" ")
      }
      println("Number of groups: " + groups.size + "   Groups:\n    " + groups.keys.toSeq.sorted.map(show).mkString("\n    "))

      if ((args.length == 3) && args(2).equalsIgnoreCase("split")) {
        def fmt(i: Int): String = {
          i.formatted(s"%0${groups.size.toString.length}d")
        }
        groups.keys.toSeq.sorted.map(groups).zipWithIndex.foreach(ig => writeGroup(fmt(ig._2 + 1), ig._1))
      } else
        println("No files created.")
    } catch {
      case t: Throwable =>
        println("Error: " + fmtEx(t))
        usage(1)
    }
  }

}
