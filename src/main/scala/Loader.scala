package hmrc.smartstub

import java.net.URL

import org.scalacheck.Gen._
import org.scalacheck._

trait Loader extends Any {
  def loadWeightedFile(file: String): Gen[String] = {
    val resource = this.getClass.getResource(file)
    val data = scala.io.Source.fromURL(resource).getLines
    val nocomments = data.filterNot(_.startsWith("#"))
    val freqTuples = nocomments.map(_.split("\t").toList).collect {
      case (f :: w :: _) => (w.filter(_.isDigit).toInt, const(f))
    }.toSeq
    frequency(freqTuples: _*)
  }

  def loadFile(file: String): Gen[String] = {
    val resource = this.getClass.getResource(file)
    val data = scala.io.Source.fromURL(resource).getLines
    oneOf(data.filterNot(_.startsWith("#")).toList)
  }

}

sealed trait Direction

case object Forwards extends Direction

case object Backwards extends Direction

case class HierarchicalFile(file: String) {
  val resource: URL = this.getClass.getResource(file)
  val raw: Iterator[String] = scala.io.Source.fromURL(resource).getLines
  val uncommented: Iterator[String] = raw.filterNot(_.startsWith("#"))
  val data: Seq[(Array[String], Int)] = uncommented.map { line =>
    val splitLine = line.split(",")
    (splitLine.init, splitLine.last.toInt)
  }.toSeq

  def apply(selection: String*): Gen[String] = {
    apply(0, Forwards, selection: _*)
  }

  def apply(depth: Int): Gen[String] = {
    apply(depth, Forwards)
  }

  def apply(direction: Direction, selection: String*): Gen[String] = {
    apply(0, direction, selection: _*)
  }

  private def apply(drop: Int, direction: Direction, selection: String*): Gen[String] = direction match {
    case Forwards => {
      frequency(data.filter(_._1.startsWith(selection)).map { case (fst, snd) =>
        (snd, const(fst.drop(selection.size + drop).head))
      }: _*)
    }
    case Backwards => {
      frequency(data.filter(_._1.endsWith(selection)).map { case (fst, snd) =>
        (snd, const(getLeft(fst, selection)))
      }: _*)
    }
  }

  private def getLeft(row: Seq[String], selection: Seq[String]): String = row.length - selection.length match {
    case 1 => row.dropRight(selection.length).head
    case _ => row.dropRight(selection.length).tail.head
  }

}
