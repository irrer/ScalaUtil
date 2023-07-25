package edu.umro.ScalaUtil

/**
  * Classic statistical functions.
  */
object Stats {

  /**
    * Calculate the first quartile of a list of values.
    * @param list Unordered list of values.
    * @return First quartile.
    */
  //noinspection ScalaWeakerAccess
  def quartile1(list: Seq[Double]): Double = {
    val sorted = list.sorted
    val size = list.size

    def q1: Double = {
      val i = size / 4
      size % 4 match {
        case 0 =>
          (sorted(i) - sorted(i - 1)) * .75 + sorted(i - 1)
        case 1 =>
          sorted(i)
        case 2 =>
          (sorted(i + 1) - sorted(i)) * .25 + sorted(i)
        case 3 =>
          (sorted(i + 1) - sorted(i)) * .5 + sorted(i)
      }
    }
    q1
  }

  /**
    * Calculate the third quartile of a list of values.
    *
    * @param list Unordered list of values.
    * @return Third quartile.
    */
  //noinspection ScalaWeakerAccess
  def quartile3(list: Seq[Double]): Double = {
    val sorted = list.sorted
    val size = list.size

    def q3: Double = {
      val i = (size * 3) / 4
      size % 4 match {
        case 0 =>
          (sorted(i) - sorted(i - 1)) * .25 + sorted(i - 1)
        case 1 =>
          sorted(i)
        case 2 =>
          (sorted(i) - sorted(i - 1)) * .75 + sorted(i - 1)
        case 3 =>
          (sorted(i) - sorted(i - 1)) * .5 + sorted(i - 1)
      }
    }
    q3
  }

  /**
    * Calculate the median ('middle') value of a list.
    *
    * Interactive page: https://www.w3schools.com/python/trypython.asp?filename=demo_ref_stat_median
    *
    * @param list Unordered list of values.
    * @return Median value.
    */
  //noinspection ScalaWeakerAccess
  def median(list: Seq[Double]): Double = {
    val sorted = list.sorted
    val size = sorted.size
    val m = size match {
      case 0 => Double.NaN
      case _ if (size % 2) == 0 =>
        val s2 = size / 2
        (sorted(s2 - 1) + sorted(s2)) / 2
      case _ => sorted(size / 2)
    }
    m
  }

  /**
    * Calculate the interquartile range of list of numbers.
    * This function interpolates between consecutive values as necessary.
    *
    * The interquartile range (IQR) is the difference between the 75th and 25th percentile of
    * the data. It is a measure of the dispersion similar to standard deviation or variance
    * but is much more robust against outliers.
    *
    * Interactive page for calculating interquartile range:
    * https://www.w3schools.com/statistics/trypython.asp?filename=demo_stat_python_iqr
    *
    * Python source reference that this was modeled on:
    * https://github.com/scipy/scipy/blob/main/scipy/stats/_stats_py.py#L3442
    *
    * @param list Unordered list of values.
    * @return Interquartile range.
    */
  //noinspection ScalaWeakerAccess
  def iqr(list: Seq[Double]): Double = {
    val sorted = list.sorted
    val size = list.size

    def q1: Double = {
      val i = size / 4
      size % 4 match {
        case 0 =>
          (sorted(i) - sorted(i - 1)) * .75 + sorted(i - 1)
        case 1 =>
          sorted(i)
        case 2 =>
          (sorted(i + 1) - sorted(i)) * .25 + sorted(i)
        case 3 =>
          (sorted(i + 1) - sorted(i)) * .5 + sorted(i)
      }
    }

    def q3: Double = {
      val i = (size * 3) / 4
      size % 4 match {
        case 0 =>
          (sorted(i) - sorted(i - 1)) * .25 + sorted(i - 1)
        case 1 =>
          sorted(i)
        case 2 =>
          (sorted(i) - sorted(i - 1)) * .75 + sorted(i - 1)
        case 3 =>
          (sorted(i) - sorted(i - 1)) * .5 + sorted(i - 1)
      }
    }

    val iqr = size match {
      case 0 => Double.NaN // handle special case of empty list
      case 1 => 0.0 // handle special case of list with size 1
      case _ => quartile3(list) - quartile1(list)
    }
    // println(s"q1: $q1    q3: $q3   iqr: $iqr")
    iqr
  }

  /**
    * For testing only.
    * @param args Not used.
    */
  def main(args: Array[String]): Unit = {

    println("from scipy import stats")

    def show(seq: Seq[Double]): Unit = {
      println
      val r = iqr(seq)
      println(s"size: ${seq.size} : ${seq.sorted.mkString(", ")}  -->  $r")
      println(s"print(stats.iqr([${seq.mkString(", ")}]))")
      println
    }

    if (true) {
      show(Seq())
      show(Seq(0.0))
      show(Seq(13.0))
      show(Seq(13.0, 21.0))
      show(Seq(13.0, 21.0, 85.0))
    }
    if (true) {
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0))
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0, 25.0))
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0, 25.0, 27.0))
      show(Seq(1.0, 2.0, 3.0, 4.0, 7.0, 10.0, 12.0, 14.0, 17.0, 19.0, 20.0, 25.0, 27.0, 33.0))
    }

    if (true) {
      for (i <- 0 until 10) {
        val seq = (0 to i + 8).map(_ => scala.util.Random.nextDouble() * 100)
        show(seq)
      }
    }

    if (true) {
      def showMed(list: Seq[Double]): Unit = {
        println(s"size: ${list.size}  ::  ${list.sorted.mkString(", ")}  -->  ${median(list)}")
      }

      showMed(Seq())
      showMed(Seq(5))
      showMed(Seq(1, 3, 5, 7, 9, 11, 13))
      showMed(Seq(1, 3, 5, 7, 9, 11))
      showMed(Seq(-11, 5.5, -3.4, 7.1, -9, 22))
    }
  }

}
