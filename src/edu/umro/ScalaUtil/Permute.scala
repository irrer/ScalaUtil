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
    private def permuteFunction[A, B](first: Seq[A], last: Seq[A], func: (Seq[A]) => Boolean): Boolean = {
        if (last.size == 1) func(first ++ last)
        else last.zipWithIndex.foldLeft(true)((s, ai) => { s && permuteFunction((first :+ ai._1), remove1(last, ai._2), func) })
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
     * be N! (N factorial), so even a modestly sized list of 20 will produce return 2432902008176640000
     * (about 2.4e18 or 2.4 quintillion) results, if you don't die of old age first.
     */
    def permute[A](seq: Seq[A]): Seq[Seq[A]] = {
        if (seq.size > 21) throw new RuntimeException("Excessively sized input of " + seq.size + " members would take years to calculate and exceed memory capacity")
        else permuteResult(Seq[A](), seq, Seq[Seq[A]]())
    }

    /**
     * For testing only
     *
     */
    def main(args: Array[String]): Unit = {
        var count = 0
        def func(seq: Seq[String]): Boolean = { count += 1; println(count.formatted("%4d ") + "funcS: " + seq); true }
        def funcStop(seq: Seq[String]): Boolean = { count += 1; println(count.formatted("%4d ") + "funcStop: " + seq); !seq(1).equals("d") }
        val seq = Seq("a", "b", "c", "d")

        count = 0
        println("process all: " + permuteFunction(Seq[String](), seq, func))
        println("\n\n\n")

        count = 0
        println("process until second member is 'd': " + permute(seq, funcStop))

        println("\n\n\n")
        count = 0
        println("Make list of all lists")
        permute(seq).map(l => func(l))
    }
}
