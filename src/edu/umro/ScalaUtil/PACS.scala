package edu.umro.ScalaUtil

/**
 * Define a DICOM device.
 */
class PACS(val aeTitle:String, val host:String, val port:Int) {
    
    def this(composite: String) = this(composite.split(":")(0),composite.split(":")(1),composite.split(":")(2).toInt);
    
    override def toString = aeTitle + " = " + host + ":" + port
}
