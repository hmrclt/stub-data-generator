package uk.gov.hmrc.smartstub

import org.scalacheck._
import Gen._

trait Loader extends Any {
  def loadWeightedFile(file: String): Gen[String] = {
    val resource = this.getClass.getResource(file)
    val data = scala.io.Source.fromURL(resource).getLines
    val nocomments = data.filterNot(_.startsWith("#"))
    val freqTuples = nocomments.map(_.split("\t").toList).collect {
      case (f::w::_) => (w.filter(_.isDigit).toInt, const(f))
    }.toSeq
    frequency(freqTuples :_*)
  }

  def loadFile(file: String): Gen[String] = {
    val resource = this.getClass.getResource(file)
    val data = scala.io.Source.fromURL(resource).getLines
    oneOf(data.filterNot(_.startsWith("#")).toList)
  }

}
