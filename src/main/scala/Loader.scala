package hmrc.smartstub

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

  def loadHierarchicalFile(file: String, selection: String*): Gen[String] = {
    val resource = this.getClass.getResource(file)
    val raw = scala.io.Source.fromURL(resource).getLines
    val uncommented = raw.filterNot(_.startsWith("#"))
    val data = uncommented.map{ line =>
      val splitLine = line.replaceAll(" ", "").split(",")
      (splitLine.init, splitLine.last.toInt)
    }.filter(_._1.startsWith(selection)).map{ case (fst,snd) => 
        (snd, const(fst.drop(selection.size).head))
    }.toSeq
    frequency(data :_*)
  }
}
