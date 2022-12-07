package edu.umro.ScalaUtil

import scala.xml.Elem

/**
  * Support visually linking HTML to the source code that generated it.  This helps
  * when modifying code to figure out what needs to be changed.
  *
  * Usage: Append '.srcRef' to any HTML (xml) element and this code will add an
  * attribute referencing the source code that generated it.
  *
  * When the accompanying javascript finds this, it can highlight the source reference.
  *
  * Example:
  *    <div style="margin: 10px;">hey</div>  ==> <div style="margin: 10px;">hey</div>.srcRef
  *
  * produces
  *
  *    <div SrcRef="main(SrcRef.scala:47)" style="margin: 10px;">hey</div>
  *
  */
object SrcRef {

  trait RefTrait[A] {
    def srcRef(x: A): Elem
  }

  implicit object SrcRefImplicit extends RefTrait[Elem] {
    def srcRef(x: Elem): Elem = {
      val stack = Thread.currentThread.getStackTrace()(4)
      x % <attr SrcRef={stack.getMethodName + "(" + stack.getFileName + ":" + stack.getLineNumber + ")"}/>.attributes
    }
  }

  implicit class SrcRefClass[A](x: A) {
    def srcRef(implicit makesRef: RefTrait[A]): Elem = {
      makesRef.srcRef(x)
    }
  }

  /**
    * Test code.
    * @param args Not used.
    */
  def main(args: Array[String]): Unit = {

    val foo = <div style="margin: 10px;">hey</div>.srcRef
    println(foo)

  }

}
