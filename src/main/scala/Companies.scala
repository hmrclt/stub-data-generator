package hmrc.smartstub


import java.net.URL

import org.scalacheck.Gen
import org.scalacheck.Gen.oneOf

import scala.collection.mutable.ListBuffer
import scala.collection.{immutable, mutable}

trait Companies extends Any {
  def company: Gen[String] = Companies._company
}

object Companies extends Loader with MarkovChain {

  val resource: URL = this.getClass.getResource("seed-company-names.txt") // todo split the txt file over lines
  val data: String = scala.io.Source.fromURL(resource).mkString
  override val map: immutable.Map[String, String] = buildMarkov(data)

  private def buildR(length: Int): String = {
    def innerBuild(l: Int, built: mutable.StringBuilder = mutable.StringBuilder.newBuilder.append(" ")): String = {
      if (l <= built.length) {
        built.toString()
      } else {
        built.append(next(built.takeRight(2).toString()))
        innerBuild(l, built)
      }
    }
    innerBuild(length)
  }

  def maybeAmpersand(companyName: String):String = companyName match {
    case c if c.split(" ").length == 2 && rndm.nextInt(10) < 5 => c.replace(" ", " & ")
    case _ => companyName
  }

  def maybeLtdGroupInt(companyName: String): String = companyName match {
    case c if rndm.nextInt(20) < 1 => c.concat(" Ltd.")
    case c if rndm.nextInt(20) < 1 => c.concat(" Group")
    case c if rndm.nextInt(20) < 1 => c.concat(" Inc.")
    case _ => companyName
  }

  def removeSingleCharWords(comapanyName: String): String = {
    comapanyName.split(" ").filter(_.length > 1).mkString(" ") // todo should capitalise here maybe
  }

  // not convinced we should finesse at all
  def finesse(companyName: String): String = {
    maybeLtdGroupInt(maybeAmpersand(removeSingleCharWords(companyName)))
  }

  private def buildCompanyName: String = {
    finesse(buildR(5 + rndm.nextInt(15)).trim)
  }

  private def companies: Seq[String] = {
    val lb = new ListBuffer[String]()
    for (i <- 1 until 500) {
      lb += buildCompanyName
    }
    //println(lb.toList)
    lb.toList
  }

  lazy val _company: Gen[String] = oneOf(companies)

}
