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
    private lazy val revBitTable = (0 until 256).map(i => revBits(i))

    /**
     * Reverse the bits of a byte.
     */
    def revBits(in: Byte): Byte = revBitTable(in & 255).toByte

    /**
     * Reverse the bits of each byte.
     */
    def revBits(in: Seq[Byte]): Seq[Byte] = in.map(b => revBits(b))

    def byteToBitString(b: Byte): String = {
        val bits = (0 until 8).foldLeft("")((t, i) => ((b >> i) & 1) + t)
        bits
    }

    /**
     * Change the ordering of each set of 2 bytes in a byte stream.
     */
    def swapBytePairs(in: Seq[Byte]): Seq[Byte] = {
        if ((in.size % 2) != 0) throw new InvalidParameterException("swapBytePairs: Size of input bytes must be a multiple of 2")
        (0 until in.size).map(i => if ((i % 2) == 0) in(i + 1) else in(i - 1))
    }

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
     */
    def swapBytes4(in: Seq[Byte], ordering: Seq[Int]): Seq[Byte] = {

        if ((in.size % 4) != 0) throw new InvalidParameterException("swapBytes4: Size of input bytes must be a multiple of 4")
        if (ordering.size != 4) throw new InvalidParameterException("swapBytes4: Size of ordering list must be exactly 4")

        val out = Array.ofDim[Byte](in.size)

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

        println("byteToBitString    5: " + byteToBitString(5.toByte))
        println("byteToBitString 0xc0: " + byteToBitString(0xc0.toByte))
        println("byteToBitString   15: " + byteToBitString(15.toByte))
        println("byteToBitString  255: " + byteToBitString(255.toByte))
        if (true) {
            val testList: List[(Byte, Byte)] =
                List(
                    (0x02.toByte, 0x40.toByte),
                    (0x05.toByte, 0xa0.toByte),
                    (0x80.toByte, 0x01.toByte),
                    (0x06.toByte, 0x60.toByte),
                    (0x66.toByte, 0x66.toByte),
                    (0x01.toByte, 0x80.toByte),
                    (0x0f.toByte, 0xf0.toByte),
                    (0xff.toByte, 0xff.toByte))
            def testRev(fwd: Byte, rev: Byte) = {
                val r = revBits(fwd.toByte)
                if (r == rev) println("success revBits " + byteToBitString(fwd) + " -> " + byteToBitString(rev))
                else println("failure revBits reversing " + byteToBitString(fwd) + "    expected: " + byteToBitString(rev) + " but got " + byteToBitString(r))
            }
            testList.map(fr => testRev(fr._1, fr._2))
        }

        val textIn = "abcdefgh"

        if (true) {
            val expected4Out = "cdabghef"
            val seq = Seq(2, 3, 0, 1)
            val out = swapBytes4(textIn.getBytes, seq)
            val actualOut = new String(out.map(b => b.toByte).toArray)
            if (actualOut == expected4Out) println("Success swapBytes4 seq: " + seq + "    textIn: " + textIn + "    out: " + actualOut)
            else println("failure swapBytes4.  expected4Out: " + expected4Out + "    Actual: " + actualOut)
        }

        if (true) {
            val expectedPairOut = "badcfehg"
            val out = swapBytePairs(textIn.getBytes)
            val actualOut = new String(out.map(b => b.toByte).toArray)
            if (actualOut == expectedPairOut) println("Success swapBytePairs  textIn: " + textIn + "    out: " + actualOut)
            else println("failure swapBytePairs.  expectedPairOut: " + expectedPairOut + "    Actual: " + actualOut)
        }
    }
}
