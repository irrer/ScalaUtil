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

package edu.umro.ScalaUtil

/**
 * Determine all possible ordering of the list members.  This always
 * produces N! (n factorial) results.  For example, the list:
 *
 *     a b c
 *
 * would produce
 *
 *     a b c
 *     a c b
 *     b a c
 *     b c a
 *     c a b
 *     c b a
 */
object Permute {

  def remove1[A](seq: Seq[A], n: Int): Seq[A] = {
    val pair = seq.splitAt(n)
    pair._1 ++ pair._2.tail
  }

  /**
   * Find all possible permutations of the members of list and apply the function to each.  If the
   * function returns true, then continue, otherwise stop.
   */
  private def permuteFunction[A](first: Seq[A], last: Seq[A], func: (Seq[A]) => Boolean): Boolean = {
    if (last.size == 1) func(first ++ last)
    else last.zipWithIndex.foldLeft(true)((s, ai) => { s && permuteFunction((first :+ ai._1), remove1(last, ai._2), func) })
  }

  /**
   * Find all possible permutations of the members of list and apply the function to each.  If the
   * function returns true, then continue, otherwise stop.
   */
  private def permuteFunctionDistinct[A, B](first: Seq[A], last: Seq[A], func: (Seq[A]) => Boolean): Boolean = {
    if (last.size == 1) func(first ++ last)
    else last.distinct.foldLeft(true)((s, ai) => { s && permuteFunctionDistinct((first :+ ai), remove1(last, last.indexOf(ai)), func) })
  }

  private def permuteResultDistinct[A, B](first: Seq[A], last: Seq[A], result: Seq[Seq[A]]): Seq[Seq[A]] = {
    val r = result
    val rf = result.flatten
    if (last.size == 1) result :+ (first ++ last)
    else last.distinct.zipWithIndex.map((ai) => {
      permuteResultDistinct(
        (first :+ ai._1),
        remove1(last, last.indexOf(ai._2)),
        result)
    }).flatten
  }

  private def permuteResult[A, B](first: Seq[A], last: Seq[A], result: Seq[Seq[A]]): Seq[Seq[A]] = {
    if (last.size == 1) result :+ (first ++ last)
    else last.zipWithIndex.map(ai => permuteResult((first :+ ai._1), remove1(last, ai._2), result)).flatten
  }

  /**
   * Find all possible permutations of the members of list and apply the function to each.  If the
   * function returns true, then continue, otherwise stop.
   *
   * @return: True if the function always returned true.
   */
  def permute[A](seq: Seq[A], func: (Seq[A]) => Boolean): Boolean = permuteFunction(Seq[A](), seq, func)

  /**
   * Find all possible permutations of the members of list and return them.  Note that the returned list will
   * be N! (N factorial), so even a modestly sized list of 20 will produce 2432902008176640000
   * (about 2.4e18 or 2.4 quintillion) results, if you don't die of old age first.
   */
  def permute[A](seq: Seq[A]): Seq[Seq[A]] = {
    if (seq.size > 21) throw new RuntimeException("Excessively sized input of " + seq.size + " members would take years to calculate and exceed memory capacity")
    else permuteResult(Seq[A](), seq, Seq[Seq[A]]())
  }

  /**
   * Find all possible distinct permutations of the members of list and apply the function to each.  If the
   * function returns true, then continue, otherwise stop.
   *
   * @return: True if the function always returned true.
   */
  def permuteDistinct[A](seq: Seq[A], func: (Seq[A]) => Boolean): Boolean = permuteFunctionDistinct(Seq[A](), seq, func)

  /**
   * Find all possible distinct permutations of the members of list and return them.
   */
  def permuteDistinct[A](seq: Seq[A]): Seq[Seq[A]] = permuteResultDistinct(Seq[A](), seq, Seq[Seq[A]]())

  /**
   * For testing only
   *
   */
  def main(args: Array[String]): Unit = {
    var count = 0
    def func(seq: Seq[String]): Boolean = { count += 1; println(count.formatted("%4d ") + "func: " + seq); true }
    def funcStop(seq: Seq[String]): Boolean = { count += 1; println(count.formatted("%4d ") + "funcStop: " + seq); !seq(1).equals("c") }
    val seq = Seq("a", "b", "c", "d")
    val seqDups = Seq("a", "b", "a", "d")
    val seqDups2 = Seq("0", "0", "1", "0")

    case class Cmpr(value: Int, name: String) {
      override def equals(o: Any) = o match {
        case that: Cmpr => {
          that.value.equals(this.value)
        }
        case _ => false
      }
      override def hashCode = value.hashCode
      override def toString = value.toString
    }
    def funcCmpr(seq: Seq[Cmpr]): Boolean = { count += 1; println(count.formatted("%4d ") + "funcCmpr: " + seq); true }

    count = 0
    println("process all: " + permuteFunction(Seq[String](), seq, func))
    println("\n\n\n")

    count = 0
    println("process until second member is 'd': " + permute(seq, funcStop))

    println("\n\n\n")
    count = 0
    println("Make list of all lists")
    permute(seq).map(l => func(l))

    println("\n\n\n")
    count = 0
    println("Distinct function list")
    permuteFunctionDistinct(Seq[String](), seq, func)

    val cmprList = List(new Cmpr(0, "a"), new Cmpr(1, "b"), new Cmpr(1, "c"), new Cmpr(0, "d"), new Cmpr(0, "e"))

    println("\n\n\n")
    count = 0
    println("Distinct function list with seqDups")
    permuteFunctionDistinct(Seq[Cmpr](), cmprList, funcCmpr)

    println("\n\n\n")

    val list2x3 = List("a", "a", "b", "b", "c", "c")
    count = 0
    println("Distinct function list with seqDups2")
    permuteFunctionDistinct(Seq[String](), list2x3, funcStop)

    count = 0
    println("Distinct function list with seqDups2")
    permuteFunction(Seq[String](), list2x3, func)

    println("Distinct result list for " + seqDups)
    permuteDistinct(seqDups2).map(s => println("    " + s))
  }
}
