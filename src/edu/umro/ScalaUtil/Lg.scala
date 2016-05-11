package edu.umro.ScalaUtil

import java.util.logging.Level
import java.util.Date
import java.util.TreeMap
import edu.umro.util.Log

/**
 * Log messages and keep them a history of them in memory.
 */

/**
 * Each 'max' value is the number of messages to keep in the history for the
 * corresponding error level.
 *
 * If a value of 0 or less is given, then keep all messages of that level.
 */
class Lg(val severeMax: Int, warningMax: Int, max: Int) {

    private val severeList = new TreeMap[Int, Message]()
    private val warningList = new TreeMap[Int, Message]()
    private val messageList = new TreeMap[Int, Message]()

    private var index = 1

    /**
     * The 'atomic unit' of storage.
     */
    class Message(val level: Level, val text: String) {
        val date = System.currentTimeMillis

        override def toString: String = new StringFormat(level).formatted("%8s") + " : " + (new Date(date)) + " | " + text
    }

    private def put(list: TreeMap[Int, Message], max: Int, msg: Message): Unit = {
        severeList.synchronized {
            list.put(index, msg)
            index = index + 1
            if (max > 0) // if 
                while (list.size > max)
                    list.remove(list.firstKey)
        }
    }

    private def add(level: Level, text: String) = {
        Log.get.log(level, text)
        val msg = new Message(level, text)
        level match {
            case Level.SEVERE => put(severeList, severeMax, msg)
            case Level.WARNING => put(warningList, warningMax, msg)
            case _ => put(messageList, max, msg)
        }
    }

    def logFinest(msg: String) = add(Level.FINEST, msg)

    def logInfo(msg: String) = add(Level.INFO, msg)

    def logWarning(msg: String) = add(Level.WARNING, msg)

    def logSevere(msg: String) = add(Level.SEVERE, msg)

    def fmtEx(throwable: Throwable): String = Log.fmtEx(throwable)

    /**
     * Get a list of messages sorted by most recent first.
     */
    def getList: List[Message] = {
        severeList.synchronized {
            val all = new TreeMap[Int, Message]()
            all.putAll(severeList)
            all.putAll(warningList)
            all.putAll(messageList)
            refArrayOps(all.values.toArray).toList.map(m => m.asInstanceOf[Message]).reverse
        }
    }

}

object Lg {

    /** Default global logger. */
    val lg = new Lg(5, 5, 200)

    def logFinest(msg: String) = lg.logFinest(msg)

    def logInfo(msg: String) = lg.logInfo(msg)

    def logWarning(msg: String) = lg.logWarning(msg)

    def logSevere(msg: String) = lg.logSevere(msg)

    def main(args: Array[String]): Unit = {
        (intWrapper(1) to 5).map(i => { lg.logInfo("happy " + i); Thread.sleep(10) })
        lg.logWarning("tense")
        Thread.sleep(1000)
        lg.logSevere("scared")

        lg.getList.map(m => println(m))
    }
}