package test.java

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Trace

import java.io.File

object SectraSort {

  private val dir = new File("""D:\tmp\scott\original\txt\0000D8BF.txt""")

  def main(args: Array[String]): Unit = {

    val lines = {
      Trace.trace()
      val fileList = FileUtil.listFiles(dir)

      def doFile(file: File): Seq[String] = {
        FileUtil.readTextFile(file).right.get.split("\n").distinct
      }

      val all = fileList.flatMap(doFile).distinct

      all
    }

    def typeOf(s: String) = s.replaceAll(":.*", "")

    val types = lines.distinct.map(typeOf)

    val ofInterest = types.filter(t => lines.count(l => typeOf(l).equals(t)) == 20).distinct

    println(ofInterest.mkString("\n"))

  }

}
