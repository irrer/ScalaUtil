package edu.umro.ScalaUtil

import java.io.File
import java.sql.Blob
import javax.sql.rowset.serial.SerialBlob
import java.util.zip.ZipOutputStream
import java.util.zip.ZipEntry
import java.io.ByteArrayOutputStream
import java.util.zip.ZipInputStream
import java.io.ByteArrayInputStream
import resource.managed
import java.io.OutputStream
import java.io.InputStream
import scala.annotation.tailrec
import edu.umro.util.Utility
import java.io.FileOutputStream
import java.io.FileInputStream

object FileUtil {

  /**
   * Copy the input stream to the output until the input is empty.  Note that the output is not closed.
   *
   * @return The number of bytes copied.
   */
  def copyStream(input: InputStream, output: OutputStream): Long = {
    val len = 8 * 1024
    val buffer = new Array[Byte](len)
    val EOF = -1
    @tailrec
    def copy(size: Long): Long = {
      val n = input.read(buffer)
      if (n == EOF) {
        size
      } else {
        output.write(buffer, 0, n)
        copy(size + n)
      }
    }
    copy(0)
  }

  /**
   * Recursively compare two directories or files and throw an exception if their content is different.
   *
   * @param a
   *            One file or directory.
   *
   * @param b
   *            The other file or directory.
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   */
  def compareDirs(a: File, b: File): Unit = {
    if (a.isDirectory && b.isDirectory) {
      val aList = a.listFiles
      val bList = b.listFiles
      if (aList.length != bList.length) throw new RuntimeException("Missing file")
      aList.map(aa => compareDirs(aa, new File(b, aa.getName())))
    } else {
      if (!(a.isFile() && b.isFile() && edu.umro.util.Utility.compareFiles(a, b))) throw new RuntimeException("files not equal");
    }
  }

  /**
   * Recursively compare two directories or files and throw an exception if
   * their content is different.  Any file access errors will result in a return of 'false';
   *
   * @param a
   *            One file or directory.
   *
   * @param b
   *            The other file or directory.
   *
   * @return True if the same, false if different.
   *
   * @throws FileNotFoundException
   * @throws IOException
   */
  def compareFolders(a: File, b: File): Boolean = {
    try {
      compareDirs(a, b);
      true
    } catch {
      case t: Throwable => false
    }
  }

  /**
   * Replace all characters in a name that are not supported by the Windows file system.
   *
   * @name: File name
   *
   * @replacement: Character to be used instead of original.
   *
   * This will work for *nix systems too.  It will replace some characters that would be
   * allowed in *nix, but using them is generally not a good idea anyway.
   */
  def replaceInvalidFileNameCharacters(name: String, replacement: Char) = {
    val original = name.getBytes
    val modified = (0 until name.size).map(i => {
      val c = name.charAt(i)
      c match {
        case _ if (c == '\\') || (c == '/') || (c == ':') || (c == '*') || (c == '"') || (c == '<') || (c == '>') => replacement
        case _ if (c > 31) && (c < 127) => c
        case _ => replacement
      }
    }).toArray
    new String(modified)
  }

  /**
   * Given a file or directory, make a byte array out of the zipped contents.  If it is a directory, include children.
   *
   * @param inFileList Zip this list of files.  If a file is a directory, then zip the whole tree below it.
   *
   * @param excludePatternList: Exclude any file whose name matches this pattern.  If the file is a directory, then its children will be excluded too.
   *
   * @param excludeFileList: Exclude any file equal on this list .  If the file is a directory, then its children will be excluded too.
   *
   * @param outputStream: Write to this stream.
   */
  def readFileTreeToZipStream(inFileList: Seq[File], excludePatternList: Seq[String], excludeFileList: Seq[File], outputStream: OutputStream): Unit = {
    def includable(file: File) = {
      val name = file.getName
      excludePatternList.find(e => name.matches(e)).isEmpty && excludeFileList.find(e => e.equals(file)).isEmpty
    }

    def addOneFileToZip(file: File, zipOut: ZipOutputStream, parentName: Option[String]): Unit = {
      if (includable(file)) {
        val entryName = if (parentName.isDefined) parentName.get + "/" + file.getName else file.getName
        if (file.isDirectory) file.listFiles.map(f => addOneFileToZip(f, zipOut, Some(entryName)))
        else {
          val data = Utility.readBinFile(file)
          val zipEntry = new ZipEntry(entryName)
          zipOut.putNextEntry(zipEntry)
          zipOut.write(data)
          zipOut.closeEntry
        }
      }
    }

    managed(new ZipOutputStream(outputStream)) acquireAndGet {
      zipOut =>
        {
          inFileList.map(inFile => addOneFileToZip(inFile, zipOut, None))
        }
    }
  }

  def readFileTreeToZipByteArray(inFileList: Seq[File], excludePatternList: Seq[String], excludeFileList: Seq[File]): Array[Byte] = {
    managed(new ByteArrayOutputStream) acquireAndGet {
      baos =>
        {
          readFileTreeToZipStream(inFileList, excludePatternList, excludeFileList, baos)
          baos.toByteArray
        }
    }
  }

  /**
   * Given a file tree, zip it up and write it to a file.
   *
   * @param mainFile: Top level input file
   */
  def readFileTreeToZipFile(inFileList: Seq[File], excludePatternList: Seq[String], excludeFileList: Seq[File], zipFile: File): Unit = {
    zipFile.delete
    managed(new FileOutputStream(zipFile)) acquireAndGet {
      fos =>
        {
          readFileTreeToZipStream(inFileList, excludePatternList, excludeFileList, fos)
          fos.close
        }
    }
  }

  /**
   * Given an input stream, write it as a zipped tree of files.
   *
   * @param inputStream: Input stream.
   *
   * @param parentDir: Put new files under this directory.
   *
   */
  def writeZipToFileTree(inputStream: InputStream, parentDir: File): Unit = {

    parentDir.mkdirs

    managed(new ZipInputStream(inputStream)) acquireAndGet {
      zipIn =>
        {
          @tailrec
          def next: Unit = {
            val entry = zipIn.getNextEntry
            if (entry != null) {
              val file = new File(parentDir, entry.getName.replace("/", File.separator))
              if (entry.isDirectory) file.mkdirs
              else {
                file.getParentFile.mkdirs
                val fos = new FileOutputStream(file)
                val size = copyStream(zipIn, fos)
                fos.close
              }
              next
            }
          }
          // Start processing
          next
        }
    }
  }

  /**
   * Given a byte array containing zipped files, expand it and write to files.
   *
   * @param byteArray: Input bytes.
   *
   * @param parentDir: Put new files under this directory.
   */
  def writeByteArrayZipToFileTree(byteArray: Array[Byte], parentDir: File): Unit = {
    managed(new ByteArrayInputStream(byteArray)) acquireAndGet {
      byteArrayIn =>
        {
          writeZipToFileTree(byteArrayIn, parentDir)
        }
    }
  }

  /**
   * Given a zip file, expand it and write to files.
   *
   * @param zipFile: Input zip file.
   *
   * @param parentDir: Put new files under this directory.
   */
  def writeZippedFileToFileTree(zipFile: File, parentDir: File): Unit = {
    managed(new FileInputStream(zipFile)) acquireAndGet {
      zipFileIn =>
        {
          writeZipToFileTree(zipFileIn, parentDir)
        }
    }
  }

  /**
   * List the files in a directory sorted alphabetically.  If the directory is empty or
   * there are errors then return an empty list.
   */
  def listFiles(dir: File): Seq[File] = {
    try {
      val list = dir.listFiles.sortBy(_.getName)
      list.toSeq
    } catch {
      case t: Throwable => Seq[File]()
    }
  }

  /**
   * Global lock for synchronizing all file writes so that only one write is being done at
   *  a time (as opposed to being done in parallel).
   */
  private val fileSystemWriteSync = "sync"

  def writeBinaryFile(file: File, data: Array[Byte]): Unit = fileSystemWriteSync.synchronized({
    file.delete
    val fos = new FileOutputStream(file)
    fos.write(data)
    fos.flush
    fos.close
  })

  def writeFile(file: File, text: String): Unit = writeBinaryFile(file, text.getBytes)

  def readBinaryFile(file: File): Either[Throwable, Array[Byte]] = {
    try {
      val fis = new FileInputStream(file)
      val buf = new Array[Byte](file.length.toInt)
      fis.read(buf)
      fis.close
      Right(buf)
    } catch {
      case t: Throwable => Left(t)
    }
  }

  def readTextFile(file: File): Either[Throwable, String] = {
    val result = readBinaryFile(file)
    if (result.isLeft) Left(result.left.get)
    else Right(new String(result.right.get))
  }

  def main(args: Array[String]): Unit = { // TODO rm
    // add comment to test git
    val j = edu.umro.ScalaUtil.FileUtil.replaceInvalidFileNameCharacters("oasijdfoaj", 'X')
    println("j: " + j)
  }

}