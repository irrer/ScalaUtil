package edu.umro.ScalaUtil

import java.io.File
import java.io.FileInputStream
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
import org.slf4j.impl.StaticLoggerBinder
import scala.collection.mutable.ArrayBuffer
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import com.pixelmed.display.ConsumerFormatImageMaker
import javax.imageio.ImageIO
import edu.umro.util.OpSys

/**
 * Fix X-Ray Angiographic files for Joann Prisciandario.
 */
object XRayAngioFixer extends Logging {

  private val logBuffer = ArrayBuffer[String]();
  private def log(msg: String) = {
    logBuffer += msg
    println(msg)
  }

  private val outputDirName = "output"
  private val htmlFileDirName = "report"
  private val htmlFileName = "index.html"
  private val logFileName = "log.txt"
  private val doctypePrefix = "<!DOCTYPE html>\n"

  private val usageMessage = {
    """
Usage:

  XRayAngioFixer [DICOM Folder]

You can drag a folder from the Windows desktop and drop it on
this program.

Results will be written to a new folder under the original folder.
Note that this means that you must be able to write to this folder.
"""
  }

  private def logFile(dir: File) = {
    val lf = new File(new File(dir, htmlFileDirName), logFileName)
    lf
  }

  private def writeLogFile(dir: File) = {
    try {
      val lf = logFile(dir)
      println("Writing log file to " + lf.getAbsolutePath)
      lf.delete
      Utility.writeFile(lf, logBuffer.mkString("", "\n", "\n").getBytes)
    } catch {
      case t: Throwable => None
    }
  }

  private def failure(t: Throwable) = {
    val msg = "Unexpected failure: " + fmtEx(t)
    println(msg)
    System.exit(1)
  }

  private def readFile(file: File): Option[AttributeList] = {
    try {
      val al = new AttributeList
      log("reading file: " + file.getAbsolutePath)
      al.read(file)
      Some(al)
    } catch {
      case t: Throwable => None
    }
  }

  private def usage(msg: String) = {
    log(msg)
    println(usageMessage)
    System.exit(1)
  }

  private def isXRayAngio(al: AttributeList): Boolean = {
    val at = al.get(TagFromName.SOPClassUID)
    (at != null) && at.getSingleStringValueOrEmptyString.trim.equals(SOPClass.XRayAngiographicImageStorage)
  }

  private def fileIsOk(file: File): Option[AttributeList] = {
    val al = readFile(file)

    file match {
      case _ if (!file.canRead) => { log("Ignoring file.  No read permission or not a regular file : " + file.getAbsolutePath); None }
      case _ if (al.isEmpty) => { log("Ignoring file.  Does not appear to be DICOM : " + file.getAbsolutePath); None }
      case _ if (!isXRayAngio(al.get)) => { log("Ignoring file.  Is not X-Ray Angiographic Image : " + file.getAbsolutePath); None }
      case _ => al
    }
  }

  /**
   * Trim the pixel data to contain just the pixels for this image, ignoring
   * any multi-framed data.  Also convert OtherByteAttributeOnDisk to the more
   * conventional OtherByteAttribute.
   */
  private def fixPixelData(fileDicom: FileDicom) = {
    val al = fileDicom.al
    val Rows = al.get(TagFromName.Rows).getIntegerValues.head
    val Columns = al.get(TagFromName.Columns).getIntegerValues.head
    val BitsAllocated = al.get(TagFromName.BitsAllocated).getIntegerValues.head
    val pixSize = Rows * Columns * ((BitsAllocated + 7) / 8)
    val PixelData = (al.get(TagFromName.PixelData)) //.asInstanceOf[OtherWordAttribute]

    PixelData match {
      case otherWord: OtherWordAttribute => {
        PixelData.setValues(PixelData.getShortValues.take(pixSize / 2))
      }
      case otherByte: OtherByteAttribute => {
        PixelData.setValues(PixelData.getByteValues.take(pixSize))
      }
      case otherByteOnDisk: OtherByteAttributeOnDisk => {
        val bytes = PixelData.getByteValues.take(pixSize)
        al.remove(TagFromName.PixelData)
        val pd = new OtherByteAttribute(TagFromName.PixelData)
        pd.setValues(bytes)
        al.put(pd)
      }
      case _ => {
        log("Unexpected type of data.  This file may have problems: " + fileDicom.file.getAbsolutePath + " : " + PixelData.getClass)
      }
    }
  }

  /**
   * Do the core work that fixes the DICOM files.
   */
  private def fixDicom(fileDicom: FileDicom): Unit = {
    val al = fileDicom.al
    val ImageType = AttributeFactory.newAttribute(TagFromName.ImageType)
    ImageType.addValue("DERIVED")
    ImageType.addValue("SECONDARY")
    ImageType.addValue("SINGLE PLANE")
    ImageType.addValue("SINGLE A")
    ImageType.addValue("REFIMAGE")
    al.put(ImageType)

    al.remove(TagFromName.StartTrim)
    al.remove(TagFromName.StopTrim)
    al.remove(TagFromName.RecommendedDisplayFrameRate)
    al.remove(TagFromName.CineRate)
    al.remove(TagFromName.FrameTime)
    al.remove(TagFromName.PositionerMotion)
    al.remove(new AttributeTag(0x0021, 0x1028))
    al.remove(new AttributeTag(0x0025, 0x1010))
    al.remove(TagFromName.NumberOfFrames)
    al.remove(TagFromName.FrameIncrementPointer)
    al.remove(TagFromName.ReferencedImageSequence)

    fixPixelData(fileDicom)
  }

  private case class FileDicom(file: File, al: AttributeList);

  /**
   * If this is DICOM, then make FileDicom, else None.
   */
  private def makeFileDicom(file: File): Option[FileDicom] = {
    val al = fileIsOk(file)
    if (al.isDefined)
      Some(new FileDicom(file, al.get))
    else
      None
  }

  private def writeDicom(outDir: File, fd: FileDicom) = {
    try {
      val fileName = {
        val old = fd.file.getName
        if (old.toLowerCase().endsWith(".dcm"))
          old
        else
          old + ".dcm"
      }
      val dicomFile = new File(outDir, fileName)

      try {
        DicomUtil.writeAttributeListToFile(fd.al, dicomFile, "XRayAngioFixer")
      } catch {
        case t: Throwable => {
          log("unable to write file:\n    " + dicomFile.getAbsolutePath + "\nDo you have permission to write to this folder?" + "\nError: " + t)
          failure(t)
        }
      }
      log("Created " + dicomFile.getAbsolutePath)
    } catch {
      case t: Throwable => {
        log("Unexpected error in writeDicom: " + t.getMessage)
        log(fmtEx(t))
        failure(t)
      }
    }

  }

  private def makeImage(htmlDir: File, fd: FileDicom) = {
    try {
      val fileName = {
        val old = fd.file.getName
        if (old.toLowerCase().endsWith(".dcm"))
          old.replaceAll("....$", ".png")
        else
          old + ".png"
      }
      val pngFile = new File(htmlDir, fileName)
      val image = ConsumerFormatImageMaker.makeEightBitImage(fd.al)
      pngFile.delete
      val fos = new FileOutputStream(pngFile)
      ImageIO.write(image, "png", fos)
      fos.flush
      fos.close

      log("created image: " + pngFile.getAbsolutePath)
    } catch {
      case t: Throwable => {
        log("Unexpected error in makeImage: " + t.getMessage)
        log(fmtEx(t))
        failure(t)
      }
    }
  }

  private def makeDicomHtml(htmlDir: File, fd: FileDicom) = {
    val dicomFileName = fd.file.getName
    try {
      val htmlFileName = {
        if (dicomFileName.toLowerCase().endsWith(".dcm"))
          dicomFileName.replaceAll("....$", ".html")
        else
          dicomFileName + ".html"
      }
      val htmlFile = new File(htmlDir, htmlFileName)
      htmlFile.delete

      val content = {
        <html>
          <head>
            <title>X-Ray Angio { dicomFileName }</title>
            <meta http-equiv="refresh" content="3600"/>
          </head>
          <body style="font-family: Arial, Helvetica, sans-serif;">
            <center>
              <p>
                <h3>DICOM File { dicomFileName }</h3>
              </p>
              <p>
                <img src={ htmlFileName.replaceAll(".html$", ".png") }/>
              </p>
            </center>
            <pre>
              { "@@@@" + DicomUtil.attributeListToString(fd.al).trim }
            </pre>
          </body>
        </html>
      }

      val text = doctypePrefix + PrettyXML.xmlToText(content).replaceAll("  *@@@@", "")
      Utility.writeFile(htmlFile, text.getBytes)
      log("created image: " + htmlFile.getAbsolutePath)
    } catch {
      case t: Throwable => {
        log("Unexpected error in makeImage: " + t.getMessage)
        log(fmtEx(t))
        failure(t)
      }
    }
  }

  def makeHtml(outputDir: File, reportDir: File) = {

    val numCreated = outputDir.listFiles.size

    def fileToHtml(fileName: String) = {
      val htmlName = fileName.replaceAll(".png$", ".html")
      <a href={ htmlName }>
        <img src={ fileName } height="100"/>
      </a>
    }

    val imageHtml = {
      reportDir.list.filter(fn => fn.endsWith(".png")).map(fn => fileToHtml(fn))
    }

    val content = {
      <html>
        <head>
          <title>X-Ray Angio Fixer Report</title>
          <meta http-equiv="refresh" content="3600"/>
        </head>
        <body style="font-family: Arial, Helvetica, sans-serif;">
          <center>
            <h2><p>X-Ray Angio Fixer Report</p></h2>
            <p>DICOM Dir: <a href={ "file:///" + outputDir.getAbsolutePath }>{ outputDir.getAbsolutePath }</a></p>
            <p>Generated: { new Date }</p>
            <p>User: { OpSys.getUser }</p>
            <p> Number of files created: { outputDir.listFiles.size }</p>
            <p><a href={ logFileName }>Log file</a></p>
            <p>Click on images to view</p>
            { imageHtml }
          </center>
        </body>
      </html>
    }

    val text = doctypePrefix + PrettyXML.xmlToText(content)

    val indexHtmlFile = new File(reportDir, htmlFileName)

    Utility.writeFile(indexHtmlFile, text.getBytes)
  }

  private def findAllChildFiles(dir: File): Seq[File] = {
    val list = dir.listFiles.toSeq
    val all = list.filter(f => f.isFile) ++ (list.filter(f => f.isDirectory).map(d => findAllChildFiles(d))).flatten
    all
  }

  private def deleteOldFiles(dirList: Seq[File]) = {
    
    val timeout = System.currentTimeMillis + (10 * 1000)
    while (dirList.map(d => d.exists).reduce(_ || _) && (System.currentTimeMillis < timeout)) {
      dirList.map(d => Utility.deleteFileTree(d))
      Thread.sleep(1000)
    }
  }

  private def fixDir(dir: File) = {
    try {
      log("Processing directory " + dir.getAbsolutePath)
      if (dir.canRead && dir.canWrite && dir.isDirectory) {
        val outDir = new File(dir, outputDirName)
        val reportDir = new File(dir, htmlFileDirName)
        deleteOldFiles(Seq(outDir, reportDir))

        outDir.mkdirs
        reportDir.mkdirs

        val fileList = findAllChildFiles(dir).sortBy(f => f.lastModified)
        Trace.trace("fileList:\n    " + fileList.map(f => f.getName).mkString("\n    "))
        val fileDicomList = fileList.map(f => makeFileDicom(f)).flatten

        fileDicomList.map(fd => fixDicom(fd))
        fileDicomList.map(fd => writeDicom(outDir, fd))

        fileDicomList.map(fd => makeImage(reportDir, fd))
        fileDicomList.map(fd => makeDicomHtml(reportDir, fd))

        makeHtml(outDir, reportDir)

      } else {
        log("Ignoring because it is not a folder or no permission to read or write: " + dir.getAbsolutePath)
      }
    } catch {
      case t: Throwable => {
        log("Unexpected error in fixDir: " + t.getMessage)
        failure(t)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      val start = System.currentTimeMillis
      println("Starting " + args.mkString(" "))
      if (args.size == 0) usage("No parameters given")

      val dirName = args.mkString(" ")
      //if (args.size > 1) usage("Only one folder may be processed at a time.")

      val inDir = new File(dirName)
      fixDir(inDir)

      writeLogFile(inDir)

      println("Elapsed ms: " + (System.currentTimeMillis - start))
      System.exit(0)
    } catch {
      case t: Throwable =>
        log("Unexpected error: " + t.getMessage)
        failure(t)
    }

  }

}
