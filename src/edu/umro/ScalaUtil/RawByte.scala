package edu.umro.ScalaUtil

import java.security.InvalidParameterException
import java.io.BufferedInputStream
import java.io.ByteArrayInputStream
import java.io.DataInputStream

/**
 * Utilities for converting raw bytes to higher order values.
 */
object RawByte {

    private def revBits(in: Int): Int = {
        val bits = (0 until 8).map(i => (in >> i) & 1)
        bits.foldLeft(0)((s, b) => (s << 1) + b)
    }

    /** Table of all possible byte values and their value when the bits are reversed. */
    private val revBitTable = (0 until 256).map(i => revBits(i))

    /**
     * Reverse the bits of a byte.
     */
    def revBits(in: Byte): Byte = revBitTable(in & 255).toByte

    /**
     * Reverse the bits of each byte.
     */
    def revBits(in: Seq[Byte]): Seq[Byte] = in.map(b => revBits(b))

    /**
     * Change the ordering of each set of 4 bytes in a byte stream.
     *
     * @param in: Data to be operated on. The size must be a multiple of 4.
     *
     * @param ordering: New order for each set of 4 bytes.  Must contain exactly 4 values.  Examples:
     *
     *     0, 1, 2, 3 : do not change order
     *
     *     2, 3, 0, 1 : swap the first and last 16 bits.
     *
     * @param rev: If true, reverse the order of the bits in each byte.
     */
    def swapBytes4(in: Seq[Byte], ordering: Seq[Int], rev: Boolean): Seq[Byte] = {

        if ((in.size % 4) != 0) throw new InvalidParameterException("swapBytes4: Size of input bytes must be a multiple of 4")
        if (ordering.size != 4) throw new InvalidParameterException("swapBytes4: Size of ordering list must be exactly 4")

        val out = {
            if (rev) revBits(in).toArray
            else Array.ofDim[Byte](in.size)
        }

        def swap(fi: Int): Unit = {
            def valof(i: Int): Byte = in(fi + i)
            (0 until 4).map(i => out(fi + ordering(i)) = valof(i))
        }

        (0 until (in.size / 4)).map(fi => swap(fi * 4))
        out.toSeq
    }

    /**
     * Convert a sequence of bytes to Floats.
     */
    def bytesToFloat(in: Seq[Byte]): IndexedSeq[Float] = {
        if ((in.size % 4) != 0) throw new InvalidParameterException("bytesToFloat: Size of input bytes must be a multiple of 4")
        val bis = new BufferedInputStream(new ByteArrayInputStream(in.toArray));
        val dis = new DataInputStream(bis)
        val data = (0 until (in.size / 4)).map(i => dis.readFloat)
        data
    }

    /**
     * For testing only
     *
     */
    def main(args: Array[String]): Unit = {
        def func(seq: Seq[String]): Unit = { println(seq) }
        val seq = Seq("a", "b", "c", "d")
        func(seq)
    }
}
