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

import java.io.File
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
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Trace
import com.pixelmed.dicom.OtherByteAttributeOnDisk
import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeList.ReadTerminationStrategy
import edu.umro.ScalaUtil.FileUtil
import java.text.SimpleDateFormat

/**
 * Count number of occurrences of files from Philips CT
 */
object PhilipsSort {

  val dateFmt = new SimpleDateFormat("yyyy-MM-dd EEE HH:mm:ss")

  val firstDate = dateFmt.parse("2019-09-01 Sun 00:00:01")
  println("Starting date: " + dateFmt.format(firstDate))

  class Ser(text: String) {
    val date = {
      try {
        val d = dateFmt.parse(text.take(23))
        if (d.getTime > firstDate.getTime)
          Some(d)
        else None
      } catch {
        case t: Throwable => None
      }
    }
    val isPhil = text.contains(" : Philips-CT : ")
    val isRecent = date.isDefined && (date.get.getTime > firstDate.getTime)
    val care = isPhil && isRecent
    override def toString = text.trim + " SrPhil: " + isPhil
  }

  class Stud(text: String) {
    val t = text
    val lineSeq = text.replace('\n', '!').split("!").filter(_.nonEmpty)
    val studyText = lineSeq.head.trim
    val seriesTextList = lineSeq.tail.mkString("\n").replaceAll("        ", "!").split("!").filter(_.nonEmpty)
    val seriesList = seriesTextList.map(st => new Ser(st))
    val hasPhil = seriesList.nonEmpty && seriesList.filter(sr => sr.isPhil).nonEmpty
    val isRecent = seriesList.nonEmpty && seriesList.filter(sr => sr.isRecent).nonEmpty
    val care = seriesList.nonEmpty && seriesList.filter(sr => sr.care).nonEmpty
    val firstDate: Option[Date] = seriesList.map(sr => sr.date).flatten.sortBy(d => d.getTime).headOption
    if (true) {
      val p = text.contains(" : Philips-CT : ")
      if (hasPhil && (!p)) {
        Trace.trace("hey")
      }
      if ((!hasPhil) && p) {
        Trace.trace("ho")
      }
    }
    override def toString = {
      "    Study: " + studyText.trim + " StPhil: " + hasPhil +
        "\n        " + seriesList.mkString("\n        ")
    }
  }

  class Pat(text: String) {
    val t = text
    val lineSeq = text.replace('\n', '!').split("!").filter(_.nonEmpty)
    val patText = lineSeq.head.replaceAll(" Studies: .*", "").trim

    val studyTextList = lineSeq.tail.mkString("\n").replaceAll("    Study: ", "!").split("!").filter(_.nonEmpty)
    val studyList = (studyTextList.map(studyText => new Stud(studyText))).filter(st => st.hasPhil && st.isRecent)

    val isRecent = studyList.nonEmpty && studyList.filter(st => st.isRecent).nonEmpty
    val firstDate = studyList.map(st => st.firstDate).flatten.sortBy(d => d.getTime).headOption

    override def toString = "Patient: " + patText + "\n" + studyList.mkString("\n    ")
  }

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis

    val file = new File("""D:\tmp\mig\xstor_index\Phil.txt""")
    val text = FileUtil.readTextFile(file).right.get
    val patTextList = text.split("!").filter(_.nonEmpty).filter(_.trim.size > 1)
    val h = patTextList.head
    val pp = new Pat(h)
    val patList = patTextList.map(p => new Pat(p)).filter(_.isRecent).sortBy(p => p.firstDate.get.getTime) //.drop(20).take(5)
    println("Number of patients: " + patList.size)
    val outText = patList.mkString("\n-------------------------------------------------------\n")
    //println(outText)

    val outFile = new File("""D:\tmp\mig\xstor_index\PhilipsSep2019andAfter.txt""")
    outFile.delete
    FileUtil.writeBinaryFile(outFile, outText.getBytes)
    println("Number of patients: " + patList.size)
  }

}