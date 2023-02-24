package edu.umro.ScalaUtil

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName

/**
  * Provide access to beam values
  * @param rtplan For this RTPLAN.
  * @param rtimage For this RTIMAGE, which must reference the RTPLAN.
  */
case class DicomBeam(rtplan: AttributeList, rtimage: AttributeList) {

  val beamAl: AttributeList = DicomUtil.getBeamOfRtimage(rtplan, rtimage).get

  /**
    * The leaf boundaries (positions of sides of leaves) for the given beam.
    */
  //noinspection ScalaUnusedSymbol
  val leafBoundaryList: Seq[Double] = {
    val list = DicomUtil.findAllSingle(beamAl, TagByName.LeafPositionBoundaries)
    if (list.isEmpty)
      Seq()
    else
      list.head.getDoubleValues.toSeq
  }

  //noinspection ScalaUnusedSymbol
  private case class PosSeq(posSeq: AttributeList) {
    private val devType: String = posSeq.get(TagByName.RTBeamLimitingDeviceType).getSingleStringValueOrNull.trim.toUpperCase
    val posList: Seq[Double] = posSeq.get(TagByName.LeafJawPositions).getDoubleValues.toSeq // toSeq to make this immutable

    val isXMlc: Boolean = devType.equals("MLCX")
    val isXJaw: Boolean = devType.equals("ASYMX") || devType.equals("X")
    val isYJaw: Boolean = devType.equals("ASYMY") || devType.equals("Y")
  }

  /** Gantry angle of RTIMAGE. */
  //noinspection ScalaUnusedSymbol
  val rtimageGantryAngle: Double = DicomUtil.findAllSingle(rtimage, TagByName.GantryAngle).head.getDoubleValues.head

  /** Collimator angle of RTIMAGE. */
  //noinspection ScalaUnusedSymbol
  val rtimageCollimatorAngle: Double = DicomUtil.findAllSingle(rtimage, TagByName.BeamLimitingDeviceAngle).head.getDoubleValues.head

  /**
    * Convert arbitrary angle in degrees to a number 360 < degrees >= 0
    */
  private def modulo360(degrees: Double): Double = {
    ((degrees % 360.0) + 360.0) % 360.0
  }

  private def angleDiff(a: Double, b: Double): Double = {
    val diff = modulo360(a - b).abs
    (if (diff <= 180) diff else diff - 360).abs
  }

  // use the ControlPointSequence that has a gantry angle closest to the gantry angle of the RTIMAGE
  private val ControlPointSequence = {
    val cpsList = DicomUtil.seqToAttr(beamAl, TagByName.ControlPointSequence)

    def absGantryAngleDiff(al: AttributeList): Double = {
      val angleAttrList = DicomUtil.findAllSingle(al, TagByName.GantryAngle)
      val angleList = angleAttrList.flatMap(attr => attr.getDoubleValues)
      val diff = angleList.map(angle => angleDiff(angle, rtimageGantryAngle).abs).min
      diff
    }

    def hasGantryAngle(al: AttributeList): Boolean = {
      val angleAttrList = DicomUtil.findAllSingle(al, TagByName.GantryAngle)
      val angleList = angleAttrList.flatMap(attr => attr.getDoubleValues)
      angleList.nonEmpty
    }

    val cps = cpsList.filter(hasGantryAngle).minBy(absGantryAngleDiff)
    cps
  }

  private val posSeq = DicomUtil.seqToAttr(ControlPointSequence, TagByName.BeamLimitingDevicePositionSequence).map(PosSeq)

  /** Position of X1 jaw, if it is defined. */
  //noinspection ScalaUnusedSymbol
  val x1Jaw: Option[Double] = posSeq.find(_.isXJaw).map(_.posList.head)

  /** Position of X2 jaw, if it is defined. */
  //noinspection ScalaUnusedSymbol
  val x2Jaw: Option[Double] = posSeq.find(_.isXJaw).map(_.posList(1))

  /** Position of Y1 jaw, if it is defined. */
  //noinspection ScalaUnusedSymbol
  val y1Jaw: Option[Double] = posSeq.find(_.isYJaw).map(_.posList.head)

  /** Position of Y2 jaw, if it is defined. */
  //noinspection ScalaUnusedSymbol
  val y2Jaw: Option[Double] = posSeq.find(_.isYJaw).map(_.posList(1))

  //noinspection ScalaUnusedSymbol
  val mlcX1PosList: Seq[Double] = posSeq.find(_.isXMlc) match {
    case Some(ps) => ps.posList.take(ps.posList.size / 2)
    case _        => Seq()
  }

  //noinspection ScalaUnusedSymbol
  val mlcX2PosList: Seq[Double] = posSeq.find(_.isXMlc) match {
    case Some(ps) => ps.posList.takeRight(ps.posList.size / 2)
    case _        => Seq()
  }

}
