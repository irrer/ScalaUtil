/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package test.java

import edu.umro.ScalaUtil.{FileUtil, Trace}

import java.io.File
import scala.annotation.tailrec

object PrintPdfCopy {

  private val inDir     = new File("""\\uhrofilespr1\PrintPDFData""")
  private val outDir    = new File("""\\txd\f$\TxDLite\UDA_Print_Files""")
  private val newOutDir = new File("""\\txd\f$\TxDLite\UDA_Print_Files""")


  private val reallyDoIt = true

  private def putFile(file: File, content: Array[Byte]): Unit = {
    if (reallyDoIt)
      FileUtil.writeBinaryFile(file, content)
    else
      println("would write " + file.getAbsolutePath)
  }

  /**
   * Return true if there is a file in the directory with the given content.
   *
   * @param content content
   * @param dir dir
   * @return
   */
  private def contentAlreadyExists(content: Array[Byte], dir: File): Boolean = {
    val fileList = FileUtil.listFiles(dir)

    def contentIsSame(file: File): Boolean = {
      (file.length == content.length) && {
        val other = FileUtil.readBinaryFile(file)
        other.isRight && (other.right.get.length == content.length) && (other.right.get sameElements content)
      }
    }

    val exists = fileList.exists(f => contentIsSame(f))
    if (!exists)
      println("Creating new file.")
    exists
  }


  private def putInDir(inPdfFile: File, dest: File): Unit = {
    val content = FileUtil.readBinaryFile(inPdfFile)
    if (content.isRight && (!contentAlreadyExists(content.right.get, dest))) {
      if (!dest.exists()) {
        if (reallyDoIt)
          dest.mkdirs()
        else
          println("Would mkdirs: " + dest.getAbsolutePath)
      }

      val sameName = new File(dest, inPdfFile.getName)
      if (!sameName.exists())
        putFile(sameName, content.right.get)
      else {
        val baseName = inPdfFile.getName.replaceAll("....$", "")
        println("Avoiding file name collision ...")

        @tailrec
        def writeFile(i: Int): Unit = {
          val f = new File(dest, baseName + i + ".PDF")
          if (f.exists())
            writeFile(i + 1)
          else
            putFile(f, content.right.get)
        }

        writeFile(1)
      }
    }
  }


  private def copyFile(inPdfFile: File): Unit = {
    println("working on PDF file: " + inPdfFile.getParentFile.getParentFile.getName + "/" + inPdfFile.getParentFile.getName + "/" + inPdfFile.getName)

    // figure out if the TxD directory exists
    val nickname = inPdfFile.getName.split("_").find(_.nonEmpty)
    if (nickname.isDefined) {
      val outD = new File(outDir, nickname.get)
      if (outD.exists())
        putInDir(inPdfFile, outD) // copy to existing TxD directory
      else
        putInDir(inPdfFile, new File(newOutDir, nickname.get)) // copy to new PrintPDF directory
    }
  }


  private def listFiles(file: File): Unit = {
    if (file.isDirectory)
      FileUtil.listFiles(file).foreach(f => listFiles(f))
    else {
      if (file.getName.toLowerCase.endsWith(".pdf")) {
        copyFile(file)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    Trace.trace()
    val start = System.currentTimeMillis()
    println("inDir: " + inDir.getAbsolutePath)
    println("outDir: " + outDir.getAbsolutePath)
    println("newOutDir: " + newOutDir.getAbsolutePath)

    listFiles(inDir)

    val elapsed = System.currentTimeMillis() - start

    Trace.trace("elapsed ms: " + elapsed)
  }
}
