package edu.umro.ScalaUtil.dicomCFind

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.network.Association
import com.pixelmed.network.FindSOPClassSCU
import com.pixelmed.network.IdentifierHandler
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS

import java.io.Closeable

abstract class DicomCFindBase(callingAETitle: String, calledPacs: PACS, retrieveList: Seq[AttributeTag]) extends Logging with Closeable {

  protected val queryRetrieveInformationModel: String
  protected val queryRetrieveLevel: String

  private def makeAssociation(): Association = {
    FindSOPClassSCU.getSuitableAssociation(calledPacs.host, calledPacs.port, calledPacs.aeTitle, callingAETitle, queryRetrieveInformationModel)
  }

  /** Current association.  */
  private var association: Option[Association] = None

  /**
    * Handle the incoming C-FIND results.
    * @param limit If defined, stop when this many items are received.
    */
  private class IdentHandler(limit: Option[Int]) extends IdentifierHandler {
    private val list = scala.collection.mutable.ArrayBuffer[AttributeList]()

    override def doSomethingWithIdentifier(attributeList: AttributeList): Unit = {
      //logger.trace("Got attributes: " + attributeList.toString().replace('\u0000', ' '));  // log null chars as blanks
      if (limit.isEmpty || (limit.isDefined && (list.size < limit.get))) {
        list += attributeList
      } else {
        val msg = "C-FIND limit of " + limit.get + " items exceeded.  Shutting down connection."
        logger.warn(msg)
        // only way to stop it is to close the association
        close()
      }
    }

    /**
      * Get the items received.  This is valid whether the limit was exceeded.
      */
    def get: Seq[AttributeList] = list
  }

  private def tagListToAttributeList(): AttributeList = {
    val al = new AttributeList
    retrieveList.map(tag => al.put(AttributeFactory.newAttribute(tag)))
    al
  }

  /**
    * Get the list of images for a given series.
    *
    * If the limit parameter is specified and the limit is exceeded, then the association is closed.
    *
    * @param qualifierList List of metadata for each slice.
    * @param limit Stop if this many slices received.
    * @return List of results.
    */
  //noinspection ScalaWeakerAccess
  def find(
      qualifierList: AttributeList,
      limit: Option[Int] = None
  ): Seq[AttributeList] = {
    val affectedSOPClass: String = queryRetrieveInformationModel

    // use a copy so as not to modify caller's copy
    val newQuery = tagListToAttributeList()
    qualifierList.values().forEach(attr => newQuery.put(attr))

    // ensure that the query level is given
    val queryLevelAt = AttributeFactory.newAttribute(TagFromName.QueryRetrieveLevel)
    queryLevelAt.addValue(queryRetrieveLevel)
    newQuery.put(queryLevelAt)

    val resultList: Seq[AttributeList] = {
      try {
        val identHandler = new IdentHandler(limit)
        if (association.isEmpty)
          association = Some(makeAssociation())
        new FindSOPClassSCU(association.get, affectedSOPClass, newQuery, identHandler)
        val list = identHandler.get
        list
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected exception while performing C-FIND: " + fmtEx(t))
          Seq[AttributeList]()
      }
    }
    resultList
  }

  /**
    * Close the association.  This should be done when the caller is done with this instance.  Alternately,
    * this can be called to stop a C-FIND in progress that is taking too much time.
    */
  override def close(): Unit = {
    try {
      if (association.isDefined)
        association.get.abort()
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected exception while closing C-FIND association: " + fmtEx(t))
    } finally {
      association = None
    }
  }

}
